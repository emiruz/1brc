module row_types
  implicit none

  type :: row_ptr
     type(row), pointer :: p => null()
  end type row_ptr
  
  type :: row
     character(len=:), allocatable :: key
     real    :: min
     real    :: max
     real    :: sum
     integer :: count
  end type row  
end module row_types

module builder
  use row_types
  implicit none

contains

  pure function str2real (str) result(f)
    character(len=*), intent(in) :: str
    integer :: i, off
    integer, parameter :: zero = ichar('0')
    real :: f
    
    off = merge (2, 1, str(1:1)=='-')
    i   = index(str(off:),'.')
    if (i == 2) then
       f = ichar(str(off:off)) - zero
    else 
       f = (ichar(str(off:off)) - zero)*10 + ichar(str(off+1:off+1)) - zero
    end if
    f = f + (ichar(str(off+i:off+i)) - zero) / 10.0
    if (off == 2) f = -f
  end function str2real
  
  subroutine update(buffer, hash_tbl)
    character(len=*), intent(in) :: buffer
    type(row_ptr), intent(inout) :: hash_tbl(:)
    character(len=1), parameter  :: cr = achar(10)
    integer :: i, j, k
    real    :: f
    
    i = 1
    do while (i <= len(buffer))
       j = index(buffer(i:), ';')
       k = index(buffer(i+j:), cr)
       f = str2real (buffer(i+j:i+j+k-2))
       call update_hash_tbl(buffer(i:i+j-2), f, hash_tbl)
       i = i+j+k
    end do
  end subroutine update

  pure function hash(key,m) result(h)
    use, intrinsic :: iso_fortran_env, only: int64
    integer(int64), parameter :: prime = 16777619_int64
    integer(int64), parameter :: basis = 2166136261_int64
    integer(int64) :: h
    character(len=*), intent(in) :: key
    integer, intent(in) :: m
    integer :: i

    h = basis
    do i = 1, len(key)
       h = ieor(h, iachar(key(i:i), int64))
       h = mod(h * prime, 2_int64**64)
    end do
    h = mod (h,m)
  end function hash
  
  subroutine update_hash_tbl(key, val, hash_tbl)
    character(len=*), intent(in) :: key
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
          allocate(character(len=len(key)) :: vals%key)
          vals%key   = key
          vals%min   = val
          vals%max   = val
          vals%sum   = val
          vals%count = 1
          exit
       else if (vals%key == key) then
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

  recursive subroutine display(hash_tbl)
    type(row_ptr), intent(in) :: hash_tbl(:)
    type(row), pointer :: vals
    integer :: i
    do i = 1, size(hash_tbl)
       if (associated(hash_tbl(i)%p)) then
          vals => hash_tbl(i)%p
          print '(A,F5.1,F5.1,F5.1)', vals%key, &
               vals%min, vals%max, vals%sum / vals%count
       end if
    end do
  end subroutine display

end module builder

program one_brc
  use row_types
  use builder
  implicit none

  integer, parameter :: buffer_size = 102400, tail_len=100
  integer, parameter :: hash_tbl_size = 65535
  character(len=:), allocatable :: buffer
  integer(kind=8) :: fd, read_size, off, start
  type(row_ptr) :: hash_tbl(hash_tbl_size)

  open(newunit=fd, file='measurements.txt', access='stream', &
       form='unformatted', status='old')
  inquire(unit=fd, size=read_size)

  do while (read_size > 0)
     allocate(character(len=min(read_size, buffer_size)) :: buffer)
     read(fd) buffer
     if (read_size <= buffer_size) then
        call update(buffer, hash_tbl)
        exit
     end if
     start = len(buffer)-tail_len
     off   = start + index(buffer(start:), achar(10))-1
     call update(buffer(1:off), hash_tbl)
     call fseek(fd, off - len(buffer), 1)
     read_size = read_size - off
     deallocate(buffer)
  end do
  call display(hash_tbl)
end program one_brc
