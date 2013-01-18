subroutine gen_alias_table (bins, pairs, probs_list, len)
! Subroutine generates an alias table
! 
! note that bins is a list of pairs of the form (probability,value)
! The sum of the probabilities in bins must be 1.
! We implement the alias table creation algorithm described by Vose (1991).
! For reference::
! 
!   Vose:      Code:
!   p_j        bins(j,1)
!   large_l    ind_large(l)
!   small_s    ind_small(s)
!   prob_j     probs_list(j)
!   'bin' j    pairs(j,1)
!   alias_j    pairs(j,2)
! 
  use mcnp_global
   
        ! subroutine argument variables
        real(dknd),dimension(1:len,1:2),intent(inout) :: bins
        integer(i4knd),dimension(1:len,1:2), intent(out) :: pairs
        real(dknd),dimension(1:len), intent(out) :: probs_list
        integer, intent(in) :: len

        ! internal variables
        integer :: largecnt, j,k,s,l
        integer,dimension(:),allocatable :: ind_small, ind_large
        real(dknd) :: n_inv 

        n_inv = 1._dknd / len
        
        ! Determine number of 'large' and 'small' bins
        largecnt = 0        
        do j=1,len
          if ( bins(j,1).gt.n_inv ) then
            largecnt = largecnt + 1
          endif
        enddo

        ALLOCATE( ind_small(1:(len-largecnt)) )
        ALLOCATE( ind_large(1:largecnt) )
                
        ! and store their indices in ind_small and ind_large
        l = 1
        s = 1
        do j=1,len
          if ( bins(j,1).gt.n_inv ) then
            ind_large(l) = j
            l = l + 1
          else
            ind_small(s) = j
            s = s + 1
          endif
        enddo

        ! Fill out pairs and prob_list
        do while (s.gt.1.and.l.gt.1)
          s = s - 1
          j = ind_small(s)
          l = l - 1
          k = ind_large(l)
          pairs(j,1) = bins(j,2)
          pairs(j,2) = bins(k,2) ! The alias bin
          probs_list(j) = bins(j,1) * len

          ! decrement the bin used as the alias
          bins(k,1) = bins(k,1) + (bins(j,1) - n_inv)

          if ( bins(k,1).gt.n_inv ) then
            ind_large(l) = k ! Redundant??
            l = l + 1
          else
            ind_small(s) = k
            s = s + 1
          endif
        enddo

        ! Handle any bins that require no alias
        do while (s.gt.1)
          s = s - 1
          j = ind_small(s)
          pairs(j,1) = bins(j,2)
          pairs(j,2) = -1 ! should never be used
          probs_list(ind_small(s)) = 1._dknd
        enddo

        ! Loop should only occur due to round-off errors
        ! Handles any bins in ind_large that require no alias
        do while (l.gt.1)
          l = l - 1
          k = ind_large(l)
          pairs(k,1) = bins(k,2)
          pairs(k,2) = -1 ! should never be used
          probs_list(ind_large(l)) = 1._dknd
        enddo

end subroutine gen_alias_table
