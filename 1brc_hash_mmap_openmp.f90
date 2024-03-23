program one_brc
  use iso_c_binding
  
  implicit none
  
  type :: row_ptr
     type(row), pointer :: p => null()
  end type row_ptr

  type :: row
     integer(1), allocatable :: key(:)
     real    :: min, max, sum
     integer :: count
  end type row
  
  interface
     integer function open(pathname,flags) bind(c,name='open')
       use iso_c_binding
       type(c_ptr), value, intent(in) :: pathname
       integer(c_int), value :: flags
     end function open

     integer function close(fd) bind(c,name='close')
       use iso_c_binding
       integer(c_int), value :: fd
     end function close

     type(c_ptr) function mmap(addr,len,prot,flags,fildes,off) bind(c,name='mmap')
       use iso_c_binding
       integer(c_size_t), value :: len, off
       integer(c_int), value :: addr, prot, flags, fildes
     end function mmap

     integer function munmap(addr, len) bind(c,name='munmap')
       use iso_c_binding
       type(c_ptr), value :: addr
       integer(c_size_t), value :: len
     end function munmap
  end interface

  integer, parameter :: parts = 8 !8 cores, 2 hyper-threads each.
  integer(4), parameter :: hash_tbl_size = 65536 ! must be power of 2.
  integer,parameter :: PROT_READ=1, MAP_PRIVATE=2, O_RDONLY=0
  character(len=16), target :: filename='measurements.txt'
  character(len=20), target :: c_filename  
  type(row_ptr) :: hash_tbl(parts, hash_tbl_size)
  type(c_ptr) :: cptr
  integer(c_size_t) :: read_size, chunk_size, begin_, end_, i, ret
  integer(c_int) :: fd = 10
  integer(kind=1), dimension(:), pointer, contiguous :: buffer
  type(c_ptr) :: c_filename_ptr

  inquire(file=filename, size=read_size)
  c_filename = trim(filename) // c_null_char
  c_filename_ptr = c_loc(c_filename)
  fd = open(c_loc(c_filename),O_RDONLY)
  cptr = mmap(0,read_size,PROT_READ,MAP_PRIVATE,fd,0_c_size_t)
  fd = close(fd)
  call c_f_pointer(cptr,buffer,[read_size])
  chunk_size = (read_size / parts) + merge(1,0, mod(read_size,parts) /= 0)

  !$OMP PARALLEL DO num_threads(parts) schedule(DYNAMIC) private(begin_,end_)
  do i = 1, parts
     begin_ = 1 + (i-1) * chunk_size
     if (begin_ < read_size) then
        end_ = min(begin_ + chunk_size-1, read_size)
        call parse(buffer, begin_, end_, hash_tbl(i,:))
     end if
  end do
  !$OMP END PARALLEL DO
  
  if (parts > 1) call foldl(hash_tbl(1,:), hash_tbl(2:,:))
  call display(hash_tbl(1,:))
  ret = munmap(cptr, read_size)

contains
  
  recursive subroutine foldl (h0, hs)
    implicit none
    type(row_ptr), intent(inout) :: h0(:)
    type(row_ptr), intent(in)  :: hs(:,:)
    type(row), pointer :: r
    integer :: i

    do i = 1, size(hs(1,:))
       if (.not. associated(hs(1,i)%p)) cycle
       r => hs(1,i)%p
       call upsert(r%key, r%min, r%max, r%sum, r%count, h0)
    end do
    if (size(hs,1) == 1) return
    call foldl(h0, hs(2:,:))
  end subroutine foldl

  pure function arr2real (ns) result(f)
    integer(1), parameter :: z = ichar('0'), m=ichar('-')
    integer(1), intent(in) :: ns(:)
    integer :: i, off
    real :: f
    
    off = merge (2, 1, ns(1) == m)
    i   = size(ns(off:)) - 1
    f   = -z + merge(ns(off)*1, (ns(off)-z)*10 + ns(off+1), i==2)
    f   = (f + (ns(off+i)-z) / 10.0) * merge(-1,1, off==2)
  end function arr2real

  subroutine parse(buffer, begin_, end_, hash_tbl)
    integer(1), parameter :: cr = 10, scol = ichar(';')
    integer(kind=1), intent(in) :: buffer(:)
    type(row_ptr), intent(inout):: hash_tbl(:)
    integer(c_size_t), intent(in) :: begin_, end_
    integer(c_size_t) :: x, i, j, k
    real    :: f

    i = merge(begin_, begin_+findloc(buffer(begin_:), cr, dim=1), begin_==1)
    do while (i-1 <= end_ .and. i <= read_size)
       j = -1 + i + findloc(buffer(i:), scol, dim=1)
       do x = 4,6
          k = j + x
          if (buffer(k)==cr) exit
       end do
       f = arr2real(buffer(j+1:k-1))
       call upsert(buffer(i:j-1), f, f, f, 1, hash_tbl)
       i = k+1
    end do
  end subroutine parse

  pure function hash(key) result(h)
    integer(1), intent(in) :: key(:)
    integer(4), parameter :: prime = int(z'01000193', 4)
    integer(4), parameter :: basis = int( z'811C9DC5', 4)
    integer(4) :: h
    integer :: i

    h = basis
    do i = 1, size(key)
       h = prime * ieor(h, transfer([key(i), 0_1,0_1,0_1], 0_4))
    end do
    h = 1 + iand(h, hash_tbl_size - 1)
  end function hash

  subroutine upsert(key, min_, max_, sum_, count_, hash_tbl)
    implicit none
    integer(1), parameter :: arr(4) = [0_1, 0_1, 0_1, 0_1]
    integer(1), intent(in) :: key(:)
    real,       intent(in) :: min_, max_, sum_
    integer,    intent(in) :: count_
    type(row_ptr), intent(inout) :: hash_tbl(:)
    type(row), pointer :: vals
    integer :: l, h, dir

    h = hash(key)
    l = size(key)
    do
       vals => hash_tbl(h)%p
       if (.not. associated(vals)) then
          allocate (vals)
          allocate(vals%key(l))
          vals%key      = key
          vals%min      = min_
          vals%max      = max_
          vals%sum      = sum_
          vals%count    = count_
          hash_tbl(h)%p => vals
          exit
       else if (size(vals%key)==l .and. &
            all(vals%key == key)) then
          vals%min   = min(vals%min, min_)
          vals%max   = max(vals%max, max_)
          vals%sum   = vals%sum + sum_
          vals%count = vals%count + count_
          exit
       else
          h = hash(transfer(h, arr))
       end if
    end do
  end subroutine upsert

  subroutine display(hash_tbl)
    implicit none
    type(row_ptr), intent(in) :: hash_tbl(:)
    type(row), pointer :: vals
    integer :: i,j
    character(len=:), allocatable :: str

    do i = 1, size(hash_tbl)
       if (associated(hash_tbl(i)%p)) then
          vals => hash_tbl(i)%p
          allocate(character(len=size(vals%key)) :: str)
          do j=1, size(vals%key)
             str(j:j) = achar(vals%key(j))
          end do
          print '(A,F5.1,F5.1,F5.1)', str, &
               vals%min, vals%max, vals%sum / vals%count
          deallocate(str)
       end if
    end do
  end subroutine display  
end program one_brc
