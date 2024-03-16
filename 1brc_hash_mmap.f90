module row_types
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
end module row_types

module builder
  use row_types
  use iso_c_binding
  implicit none

contains

  pure function arr2real (ns) result(f)
    integer(1), intent(in) :: ns(:)
    integer :: i, off
    integer(1), parameter :: z = ichar('0'), m=ichar('-')
    real :: f
    
    off = merge (2, 1, ns(1) == m)
    i   = size(ns(off:)) - 1
    f   = -z + merge(ns(off)*1, (ns(off)-z)*10 + ns(off+1), i==2)
    f   = (f + (ns(off+i)-z) / 10.0) * merge(-1,1, off==2)
  end function arr2real
  
  subroutine update(buffer, length, hash_tbl)
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
       call update_hash_tbl(buffer(i:j-1), f, hash_tbl)
       i = i + k
    end do
  end subroutine update

  pure function hash(key, m) result(h)
    use, intrinsic :: iso_fortran_env, only: int64
    integer(1), intent(in) :: key(:)
    integer, intent(in) :: m
    integer(int64), parameter :: prime = 16777619_int64
    integer(int64), parameter :: basis = 2166136261_int64
    integer(int64) :: h
    integer :: i

    h = basis
    do i = 1, size(key)
       h = ieor(h, iachar(achar(key(i)),int64))
       h = mod(h * prime, 2_int64**32)
    end do
    h = mod (h,m)
  end function hash
  
  subroutine update_hash_tbl(key, val, hash_tbl)
    integer(1), intent(in) :: key(:)
    type(row_ptr), intent(inout) :: hash_tbl(:)
    type(row), pointer :: vals
    integer :: h, l
    real, intent(in) :: val

    l = size(hash_tbl)
    h = hash(key, l)
    do
       vals => hash_tbl(h)%p
       if (.not. associated(vals)) then
          allocate (hash_tbl(h)%p)
          vals => hash_tbl(h)%p
          allocate(vals%key(size(key)))
          vals%key   = key
          vals%min   = val
          vals%max   = val
          vals%sum   = val
          vals%count = 1
          exit
       else if (size(vals%key)==size(key) .and. all(vals%key==key)) then
          if (val < vals%min) vals%min = val
          if (val > vals%max) vals%max = val
          vals%sum   = vals%sum + val
          vals%count = vals%count + 1
          exit
       else
          h = mod(h+1, l)
       end if
    end do
  end subroutine update_hash_tbl

  subroutine display(hash_tbl)
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

end module builder

program one_brc
  use row_types
  use builder
  use iso_c_binding
  implicit none

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

  integer, parameter :: hash_tbl_size = 65535
  type(row_ptr) :: hash_tbl(hash_tbl_size)
  type(c_ptr) :: cptr
  integer(c_size_t) :: read_size, off=0
  integer,parameter :: PROT_READ=1, MAP_PRIVATE=2, O_RDONLY=0
  integer(c_int) :: fd = 10
  integer(kind=1), dimension(:), pointer, contiguous :: buffer
  integer(8) :: iostat, ret, j, pos_s, pos_nl, cpos, pos_s1
  character(len=16), target :: filename='measurements.txt'
  character(len=20), target :: c_filename
  type(c_ptr) :: c_filename_ptr
  integer(8) :: count,i

  count = 0
  
  inquire(file=filename, size=read_size)
  c_filename = trim(filename) // c_null_char
  c_filename_ptr = c_loc(c_filename)
  fd = open(c_loc(c_filename),O_RDONLY)
  cptr = mmap(0,read_size,PROT_READ,MAP_PRIVATE,fd,off)
  fd = close(fd)
  call c_f_pointer(cptr,buffer,[read_size])
  call update(buffer, read_size, hash_tbl)
  call display(hash_tbl)
  ret=munmap(cptr, read_size)

end program one_brc
