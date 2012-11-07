!+
! Whooo!
!
module methods

        use mcnp_global

contains

subroutine gen_table(len)
!
      !use mcnp_global
      integer :: len, i
      real(dknd) :: tot
      real(dknd),dimension(1:len) :: nums
      !
      real(dknd),dimension(1:len,1:2) :: bins
      integer(i4knd),dimension(1:len,1:2) :: pairs
      real(dknd),dimension(1:len) :: probs_list
      !
      integer :: t1s, t2s, cr, cm
      real :: t1, t2, rate

      ! First initialize the system_clock
      CALL system_clock(count_rate=cr)
      CALL system_clock(count_max=cm)
      rate = REAL(cr)

      do i=1,len
        nums(i) = rang()
      enddo
        
      tot = sum(nums)

      do i=1,len
        bins(i,1) = nums(i) / tot
        bins(i,2) = i
      enddo

      call CPU_TIME(t1)
      call SYSTEM_CLOCK(t1s)

      call gen_alias_table(bins, pairs, probs_list, len)

      call CPU_TIME(t2)
      call SYSTEM_CLOCK(t2s)

      write(*,*) "For ", len , " cpu time of    ", t2-t1
      !write(*,*) "For ", len , " system time of ", (t2s-t1s)/rate

end subroutine gen_table

end module methods


program benchmark

      !
      use methods
        real :: t1, t2
        integer :: i,j

        call RN_init_problem() ! init random number generated to defaults

        !call gen_table(10000) 
        do i=1,10
          do j=1,10
            call gen_table(i*10000)
          enddo
        enddo
        !call gen_table(10000) 
        do i=1,10
          do j=2,10
            call gen_table(i*100000)
          enddo
        enddo

end program benchmark
