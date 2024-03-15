module trie_types
  implicit none

  type :: trie_ptr
     type(trie), pointer :: p => null()
  end type trie_ptr
  
  type :: trie
     real :: min = 1000
     real :: max = -1000
     real :: sum = 0
     integer :: count = 0
     type(trie_ptr) :: values(256)
  end type trie  
end module trie_types

module builder
  use trie_types
  implicit none

contains

  function str2real (str) result(f)
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
  
  subroutine update(buffer, root)
    character(len=*), intent(in) :: buffer
    character(len=1), parameter :: cr = achar(10)
    type(trie), pointer, intent(inout) :: root
    type(trie), pointer :: current
    type(trie_ptr), pointer :: temp_ptr
    integer :: i, j, k, l
    character(len=1) :: c
    real :: f
    i = 1
    l = len(buffer)
    current => root
    do while (i <= l)
       c = buffer(i:i)
       if (c == ';') then
          j = i+1
          k = index(buffer(j:), cr)
          f = str2real(buffer(j:j+k-2))
          i = j+k
          call update_stats(current, f)
          current => root
       else
          temp_ptr => current%values(ichar(c))
          if (.not. associated(temp_ptr%p)) allocate(temp_ptr%p)
          current => temp_ptr%p
          i = i + 1
       end if
    end do
  end subroutine update

  subroutine update_stats(current, f)
    type(trie), pointer, intent(inout) :: current
    real, intent(in) :: f
    if (f < current%min) current%min = f
    if (f > current%max) current%max = f
    current%sum   = current%sum + f
    current%count = current%count + 1    
  end subroutine update_stats

  recursive subroutine display(tree, key)
    type(trie), pointer, intent(in) :: tree
    character(len=*), intent(in), optional :: key
    integer :: i
    character(len=1) :: c
    
    if (tree%min < 100) then
       print '(A,A,F5.1,F5.1,F5.1)', key, ";", &
            tree%min, tree%max, tree%sum / tree%count
    endif
    do i = 1, 256
        if (associated(tree%values(i)%p)) then
            c = achar(i)
            if (present(key)) then
                call display(tree%values(i)%p, key // c)
            else
                call display(tree%values(i)%p, c)
            endif
        endif
    end do
end subroutine display

end module builder

program one_brc
  use trie_types
  use builder
  implicit none

  integer, parameter :: buffer_size = 102400, tail_len=100
  character(len=:), allocatable :: buffer
  integer(kind=8) :: fd, read_size, off, start
  type(trie), pointer :: tree

  open(newunit=fd, file='measurements.txt', access='stream', &
       form='unformatted', status='old')
  inquire(unit=fd, size=read_size)

  allocate(tree)
  do while (read_size > 0)
     allocate(character(len=min(read_size, buffer_size)) :: buffer)
     read(fd) buffer
     if (read_size <= buffer_size) then
        call update(buffer, tree)
        exit
     end if
     start = len(buffer)-tail_len
     off   = start + index(buffer(start:), achar(10))-1
     call update(buffer(1:off), tree)
     call fseek(fd, off - len(buffer), 1)
     read_size = read_size - off
     deallocate(buffer)
  end do  
  call display(tree)
end program one_brc
