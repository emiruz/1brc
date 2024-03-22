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

  integer(4), parameter :: hash_tbl_size = 65536 ! must be power of 2.
  integer,parameter :: PROT_READ=1, MAP_PRIVATE=2, O_RDONLY=0
  character(len=16), target :: filename='measurements.txt'
  character(len=20), target :: c_filename  
  type(row_ptr) :: hash_tbl(hash_tbl_size)
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
  call parse(buffer, read_size, hash_tbl)
  call display(hash_tbl)
  ret = munmap(cptr, read_size)

contains

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

  subroutine parse(buffer, length, hash_tbl)
    integer(1), parameter :: cr = 10, scol = ichar(';')
    integer(kind=1), intent(in) :: buffer(:)
    type(row_ptr), intent(inout):: hash_tbl(:)
    integer(c_size_t), intent(in) :: length
    integer(c_size_t) :: x, i, j, k
    real    :: f
    
    i = 1
    do while (i <= length)
       k = findloc(buffer(i:), cr, dim=1)
       do x = 4,6
          j = i + k - 1 - x
          if (buffer(j)==scol) exit
       end do
       f = arr2real(buffer(j+1:i+k-2))
       call upsert(buffer(i:j-1), f, f, f, 1, hash_tbl)
       i = i + k
    end do
  end subroutine parse

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

  subroutine upsert(key, min_, max_, sum_, count_, hash_tbl)
    implicit none
    integer(1), parameter :: arr(4) = [0_1, 0_1, 0_1, 0_1]
    integer(1), intent(in) :: key(:)
    real,       intent(in) :: min_, max_, sum_
    integer,    intent(in) :: count_
    type(row_ptr), intent(inout) :: hash_tbl(:)
    type(row), pointer :: vals
    integer :: l, h, dir

    h = hash(key); l = size(key)
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
end program one_brc
