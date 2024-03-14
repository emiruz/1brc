module trie_types
  implicit none
  
  type :: trie
     character(len=1) :: key = '!'
     real :: min = 1000
     real :: max = -1000
     real :: sum = 0
     integer :: count = 0
     type(trie), pointer :: child => null()
     type(trie), pointer :: sibling => null()
  end type trie  
end module trie_types

module builder
  use trie_types
  implicit none

contains

  function str2real (str) result(f)
    character(len=*), intent(in) :: str
    integer :: i, off
    integer :: zero
    real :: f
    zero = ichar('0')
    off = merge (2, 1, str(1:1)=='-')
    i = index(str(off:),'.')
    if (i==2) then
       f = ichar(str(off:off)) - zero
    else 
       f = (ichar(str(off:off)) - zero)*10 + ichar(str(off+1:off+1)) - zero
    end if
    f = f + (ichar(str(off+i:off+i)) - zero) / 10.0
    f = merge(-f,f,off==2)
  end function str2real
  
  subroutine update(buffer, root)
    character(len=*), intent(in) :: buffer
    type(trie), pointer, intent(inout) :: root
    type(trie), pointer :: current
    integer :: i, j, k
    character(len=1) :: c
    real :: f
    i = 1
    current => root
    do while (i <= len(buffer))
       c = buffer(i:i)
       if (c == ';') then
          j = i+1
          k = index(buffer(j:), achar(10))
          f = str2real (buffer(j:j+k-2))
          i = j+k
          call update_stats(current, f)
          current => root
       else if (.not. associated(current%child)) then
          allocate(current%child)
          current => current%child
          current%key = c
          i = i + 1
       else if (current%child%key == c) then
          current => current%child
          i = i + 1
       else
          current => current%child
          call find_sibling (current, c)
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
  
  subroutine find_sibling(current, c)
    character(len=1), intent(in) :: c
    type(trie), pointer, intent(inout) :: current
    do
       if (.not. associated(current%sibling)) then
          allocate(current%sibling)
          current => current%sibling
          current%key = c
          exit
       endif
       current => current%sibling
       if (current%key == c) exit
    end do
  end subroutine find_sibling
  
  recursive subroutine display(tree, prefix)
    type(trie), pointer, intent(in) :: tree
    character(len=*), intent(in), optional :: prefix
    character(len=:), allocatable :: new_prefix

    if (.not. associated(tree)) return
    if (present(prefix)) then
        new_prefix = prefix // tree%key
    else
        new_prefix = tree%key
    end if
    if (tree%min < 100) then
       print '(A,A,F5.1,F5.1,F5.1)', new_prefix, ";", &
            tree%min, tree%max, tree%sum / tree%count
    end if
    if (associated(tree%child)) then
        call display(tree%child, new_prefix)
    end if
    if (associated(tree%sibling)) then
        call display(tree%sibling, prefix)
    end if
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

  open(newunit=fd, file='measurements3.txt', access='stream', form='unformatted')
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
  call display(tree%child)
end program one_brc
