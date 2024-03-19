program one_brc
  use iso_c_binding
  
  implicit none
  
  type :: row_ptr
     type(row), pointer :: p => null()
  end type row_ptr

  type :: row
     integer(1), allocatable :: key(:)
     real    :: min
     real    :: max
     real    :: sum
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
       integer(c_int), value :: addr
       integer(c_size_t), value :: len
       integer(c_int), value :: prot
       integer(c_int), value :: flags
       integer(c_int), value :: fildes
       integer(c_size_t), value :: off
     end function mmap

     integer function munmap(addr, len) bind(c,name='munmap')
       use iso_c_binding
       type(c_ptr), value :: addr
       integer(c_size_t), value :: len
     end function munmap
  end interface

  integer, parameter :: parts = 16 !8 cores, 2 hyper-threads each.
  integer(8), parameter :: hash_tbl_size = 65536
  integer,parameter :: PROT_READ=1, MAP_PRIVATE=2, O_RDONLY=0
  character(len=16), target :: filename='measurements.txt'
  character(len=20), target :: c_filename  
  type(row_ptr) :: hash_tbl(parts, hash_tbl_size)
  type(c_ptr) :: cptr
  integer(c_size_t) :: read_size, off=0
  integer(c_int) :: fd = 10
  integer(kind=1), dimension(:), pointer, contiguous :: buffer
  integer(8) :: i, ret
  type(c_ptr) :: c_filename_ptr
  integer(8), allocatable :: begins(:), ends(:), begin_, end_
  
  inquire(file=filename, size=read_size)
  c_filename = trim(filename) // c_null_char
  c_filename_ptr = c_loc(c_filename)
  fd = open(c_loc(c_filename),O_RDONLY)
  cptr = mmap(0,read_size,PROT_READ,MAP_PRIVATE,fd,off)
  fd = close(fd)
  call c_f_pointer(cptr,buffer,[read_size])

  call chunk(buffer, read_size, parts, begins, ends)

  !$OMP PARALLEL DO num_threads(parts) schedule(DYNAMIC)
  do i = 1, parts
     begin_ = begins(i); end_ = ends(i)
     call parse(buffer(begin_:end_), 1+end_-begin_, hash_tbl(i,:))
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

  subroutine chunk(buffer, length, n, begins, ends)
    implicit none
    integer, parameter :: tail = 100 ! Problem for very small files!
    integer(1), intent(in) :: buffer(:)
    integer(c_size_t), intent(in) :: length
    integer(4), intent(in) :: n
    integer(8), allocatable, intent(out) :: begins(:), ends(:)
    integer(8) :: x, p, i, j, k

    allocate(begins(n)); allocate(ends(n))
    i = 1; p = (length-1) / n
    do k = 1, n-1
       begins(k) = i
       j = i+p
       i = j-tail
       x = findloc(buffer(i:j), 10, dim=1)
       ends(k) = i+x-1
       i = i+x
    end do
    begins(n)=i; ends(n) = length

  end subroutine chunk

  pure function arr2real (ns) result(f)
    implicit none
    integer(1), intent(in) :: ns(:)
    integer :: i, off
    integer(1), parameter :: z = ichar('0'), m=ichar('-')
    real :: f
    
    off = merge (2, 1, ns(1) == m)
    i   = size(ns(off:)) - 1
    f   = -z + merge(ns(off)*1, (ns(off)-z)*10 + ns(off+1), i==2)
    f   = (f + (ns(off+i)-z) / 10.0) * merge(-1,1, off==2)
  end function arr2real
  
  subroutine parse(buffer, length, hash_tbl)
    implicit none
    integer(kind=1), intent(in) :: buffer(:)
    type(row_ptr), intent(inout):: hash_tbl(:)
    integer(c_size_t), intent(in) :: length
    integer(1), parameter :: cr = 10, scol = ichar(';')
    integer(c_size_t) :: x, i, j, k
    real    :: f
    integer :: offs(3) = [5,6,4]
    
    i = 1
    do while (i <= length)
       k = findloc(buffer(i:), cr, dim=1)
       do x = 1,3
          j = i + k - 1 - offs(x)
          if (buffer(j)==scol) exit
       end do
       f = arr2real(buffer(j+1:i+k-2))
       call upsert(buffer(i:j-1), f, f, f, 1, hash_tbl)
       i = i + k
    end do
  end subroutine parse

  pure function hash(key) result(h)
    use, intrinsic :: iso_fortran_env, only: int8, int32
    implicit none
    integer(1), intent(in) :: key(:)
    integer(int32), parameter :: prime = int(z'01000193', int32)
    integer(int32), parameter :: basis = int( z'811C9DC5', int32)
    integer(int32) :: h
    integer :: i

    h = basis
    do i = 1, size(key)
       h = prime * ieor(h, transfer([key(i), 0_int8,0_int8,0_int8], 0_int32))
    end do
    h = mod(abs(h), hash_tbl_size)
  end function hash

  subroutine upsert(key, min_, max_, sum_, count_, hash_tbl)
    implicit none
    integer(1), parameter :: arr(4) = [0_1, 0_1, 0_1, 0_1]
    integer(1), intent(in) :: key(:)
    real,       intent(in) :: min_, max_, sum_
    integer,    intent(in) :: count_
    type(row_ptr), intent(inout) :: hash_tbl(:)
    type(row), pointer :: vals
    integer :: h, dir

    h = hash(key)
    do
       vals => hash_tbl(h)%p
       if (.not. associated(vals)) then
          allocate (vals)
          allocate(vals%key(size(key)))
          vals%key      = key
          vals%min      = min_
          vals%max      = max_
          vals%sum      = sum_
          vals%count    = count_
          hash_tbl(h)%p => vals
          exit
       else if (size(vals%key)==size(key) .and. &
            all(vals%key==key)) then
          if (min_ < vals%min) vals%min = min_
          if (max_ > vals%max) vals%max = max_
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
