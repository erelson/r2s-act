subroutine gen_alias_table(bins, pairs, probs_list, len)
! note that bins is a list of pairs of the form (probability,value)
!  The sum of the probabilities in bins must be 1.
  use mcnp_global
   
        ! subroutine argument variables
        real(dknd),dimension(1:len,1:2),intent(inout) :: bins
        integer(i4knd),dimension(1:len,1:2), intent(out) :: pairs
        real(dknd),dimension(1:len), intent(out) :: probs_list
        integer, intent(in) :: len

        ! internal variables
        real(dknd) :: n_inv 

        ! do an initial sort
        call heap_sort(bins, len)
        call CPU_TIME(th)

        ! With each pass through the following loop, we ignore another bin
        !  (the j'th bin) by setting its probability vaue to -1.
        ! pairs stores the two possible values for each alias table bin
        ! probs_list stores the probability of the first value in the
        !  alias table bin being used
        n_inv = (1._dknd/len)

        do j=1,len

          ! resort last bin
          call sort_for_alias_table(bins, len)

          ! Lowest bin is less than 1/n, and thus needs a second bin with
          !  which to fill the alias bin.
          if ( bins(j,1).lt.n_inv ) then
            probs_list(j) = bins(j,1) * len
            pairs(j,1) = bins(j,2)

            ! don't need to store second probability
            pairs(j,2) = bins(len,2)

            bins(len,1) = bins(len,1) - (n_inv - bins(j,1))
            
          ! Lowest bin should never have a probablity > 1/n, I think?
          else if (bins(j,1).gt.n_inv) then
            !write(*,*) "Problem generating alias table. See source.F90"
            pairs(j,1) = bins(j,2)
            pairs(j,2) = 0
            probs_list(j) = 1.0

            bins(j,1) = bins(j,1) - n_inv

          ! Lowest bin is exactly 1/n
          else ! (bins(j,1).eq.1) ! single possible value for given bin
            probs_list(j) = 1.0
            pairs(j,1) = bins(j,2)
            pairs(j,2) = 0

          endif

          bins(j,1) = -1.3 ! Immunity to sorting for already-used bins

        enddo

end subroutine gen_alias_table


subroutine sort_for_alias_table(bins, length)
! subroutine locates where to move the last bin in bins to,
! such that bins is presumably completely sorted again.
  use mcnp_global

        integer,intent(IN) :: length
        real(dknd),intent(INOUT),dimension(1:length,1:2) :: bins

        ! Method's variables
        integer :: cnt, i
        real(dknd),dimension(1,1:2) :: temp

        if (bins(length,1).lt.bins(length-1,1)) then
 
          ! The logic in this do loop may be problematic at 
          !  cnt = length or cnt = 1...
          do cnt=length,1,-1
            if (bins(length,1).ge.bins(cnt-1,1)) exit
          enddo
        call CPU_TIME(ta)
          ! found bin

          temp(1,1:2) = bins(length,1:2)
          bins(cnt+1:length,1:2) = bins(cnt:length-1,1:2)
          bins(cnt,1:2) = temp(1,1:2)

          !remixed
        call CPU_TIME(tb)
        tsum = tsum + (tb-ta)

        else
                continue
        endif

end subroutine sort_for_alias_table


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Heap Sort Algorithm
!  The below methods (heap_sort, heapify, sifDown, doSwap)
!   implement the heap sort algorithm as described on the Wikipedia page
!   for 'Heap Sort' circa May 2012.
!  This algorithm is used for initially sorting data before creating
!   an alias table.  The code is included in this file to avoid the need
!   to make modifications to default compiling procedures for MCNP
!   (e.g. no need to add additional source files to a makefile)

subroutine heap_sort(a, len)
! Method implements the heap sort algorithm with subroutines
! heapify and siftDown.
  use mcnp_global

        real(dknd),dimension(1:len,1:2),intent(INOUT) :: a
        integer,intent(IN) :: len

        integer :: ende ! Beyond ende, the list is sorted
                        ! Before ende, the list is a binary heap

        call heapify(a, len)

        ende = len

        do while (ende.gt.1)
          ! putting the root (aka top aka largest value) of heap at end
          call doSwap(a, len, ende, 1)

          ende = ende - 1
          ! 
          call siftDown(a, len, 1, ende)
          
        enddo

end subroutine heap_sort


subroutine heapify(a, len)
! Method creates a binary heap from an unsorted list
  use mcnp_global

        real(dknd),dimension(1:len,1:2),intent(INOUT) :: a
        integer,intent(IN) :: len

        integer :: start

        start = len / 2 ! last parent node; note indexing is from 1

        do while (start.ge.1)
          call siftDown(a, len, start, len)
          start = start - 1
        enddo

end subroutine heapify


subroutine siftDown(a, len, start, ende)
! siftDown compares a parent with its two child and swaps
! with the larger child if either is greater than the parent
  use mcnp_global

        real(dknd),dimension(1:len,1:2),intent(INOUT) :: a
        integer,intent(IN) :: len, start, ende

        integer :: root, child, swap

        root = start

        do while ( (root * 2).le.ende)
          child = root * 2
          swap = root

          ! if first child is larger than the parent
          if (a(swap,1).lt.a(child,1)) then
            swap = child
          endif

          ! if 2nd child exists and 2nd child is larger than first child
          if ( ((child+1).le.ende).and.(a(swap,1).lt.a(child+1,1)) ) then
            swap = child + 1
          endif

          if (swap.ne.root) then
            call doSwap(a, len, root, swap)
            root = swap
          else
            root = ende+1
          endif

        enddo

end subroutine siftDown


subroutine doSwap(a, len, i, j)
! Method swaps the elements in array a at positions i and j.
  use mcnp_global

        integer,intent(IN) :: len, i, j
        real(dknd),dimension(1:len,1:2),intent(INOUT) :: a

        real(dknd),dimension(1:2) :: temp

        temp(1:2) = a(i,1:2)
        a(i,1:2) = a(j,1:2)
        a(j,1:2) = temp

end subroutine doSwap 
