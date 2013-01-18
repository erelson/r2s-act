subroutine source_setup
! subroutine handles parsing of the 'gammas' file and related initializations
  use source_data
   
 
        CLOSE(50)
        OPEN(unit=50,form='formatted',file=gammas_file)

        ! Read first 5 lines of gammas
        call read_header(50)

        ! Look for parameters line, and read parameters if found.
        call read_params(50)

        ! If ergs flag was found, we call read_custom_ergs.  Otherwise 
        !  we use default energies.
        if (ergs.eq.1) then
          call read_custom_ergs(50)
          write(*,*) "The following custom energy bins are being used:"
          do i=1,n_ener_grps
            write(*,'(2es11.3)') my_ener_phot(i), my_ener_phot(i+1)
            enddo
        else ! use default energy groups; 42 groups
          n_ener_grps = 42
          ALLOCATE(my_ener_phot(1:n_ener_grps+1))
          my_ener_phot = (/0.0,0.01,0.02,0.03,0.045,0.06,0.07,0.075,0.1,0.15, &
            0.2,0.3,0.4,0.45,0.51,0.512,0.6,0.7,0.8,1.0,1.33,1.34,1.5, &
            1.66,2.0,2.5,3.0,3.5,4.0,4.5,5.0,5.5,6.0,6.5,7.0,7.5,8.0, &
            10.0,12.0,14.0,20.0,30.0,50.0/)
        endif

        ! Prepare to read in spectrum information
        ! set the spectrum array to: # of mesh cells * # energy groups
        ALLOCATE(spectrum(1:n_mesh_cells, 1:bias + n_ener_grps))
        ALLOCATE(tot_list(1:n_mesh_cells))
        if (bias.eq.1) ALLOCATE(bias_list(1:n_mesh_cells))
         
        ! reading in source strength and alias table for each voxel 
        i = 1 ! i keeps track of # of voxel entries
        do
          read(50,*,iostat=stat) (spectrum(i,j), j=1,bias + n_ener_grps)
          if (stat.ne.0) then
            i = i - 1
            exit ! exit the do loop
          endif
          if (bias.eq.1) bias_list(i) = spectrum(i,bias+n_ener_grps)
          i = i + 1
        enddo
        
        ! Check for correct number of voxel entries in gammas file.
        if (i.ne.n_mesh_cells) write(*,*) 'ERROR: ', i, ' voxels found in ' // &
                        'gammas file. ', n_mesh_cells, ' expected.'

        CLOSE(50)
        WRITE(*,*) 'Reading gammas file completed!'

        ALLOCATE(ergPairs(1:n_mesh_cells, 1:n_ener_grps, 1:2))
        ALLOCATE(ergPairsProbabilities(1:n_mesh_cells, 1:n_ener_grps))

        ! Create bins list.  Depending on if cumulative probabilities
        !  were supplied, convert to individual bin probabilities so that
        !  alias table of erg bins can be generated, too.
        if (cumulative.eq.1) then
          do i=1,n_mesh_cells
            tot_list(i) = spectrum(i,n_ener_grps)
            do j=n_ener_grps,2,-1
              spectrum(i,j) = (spectrum(i,j) - spectrum(i,j-1)) / tot_list(i)
            enddo
            spectrum(i,1) = spectrum(i,1) / tot_list(i)
            call gen_erg_alias_table (n_ener_grps, spectrum(i,1:n_ener_grps), &
                        ergPairs(i,1:n_ener_grps,1:2), &
                        ergPairsProbabilities(i,1:n_ener_grps))
          enddo
        else
          do i=1,n_mesh_cells
            tot_list(i) = sum(spectrum(i,1:n_ener_grps))
            do j=1,n_ener_grps
              spectrum(i,j) = spectrum(i,j) / tot_list(i)
            enddo
            call gen_erg_alias_table (n_ener_grps, spectrum(i,1:n_ener_grps), &
                        ergPairs(i,1:n_ener_grps,1:2), &
                        ergPairsProbabilities(i,1:n_ener_grps))
          enddo
        endif

        ! We calculate the number of source voxels.
        n_source_cells = 0
        do i=1,n_mesh_cells
          if (tot_list(i).gt.0) n_source_cells = n_source_cells + 1
        enddo
        write(*,*) "n_mesh_cells:", n_mesh_cells, &
                "n_source_cells:", n_source_cells

        ! Generate alias table of voxels if needed
        if (samp_vox.eq.1) then
          call gen_voxel_alias_table
        endif

        ! Create new debug output file if debugging is enabled.
        if (debug.eq.1) then
          OPEN(UNIT=57, FILE="source_debug", ACCESS="APPEND", STATUS="REPLACE")
          CLOSE(57)
        endif

end subroutine source_setup


subroutine read_header (myunit)
! Read in first 5 lines of gammas file
! 
! These lines contain the x,y,z mesh intervals
! and the list of active materials
! 
! Also skips over any comment lines (beginning with # character) at start of
! file.
! 
  use source_data
        
        integer,intent(IN) :: myunit
        character :: letter
        character*30 :: commentline

        ! initialize an empty 'activated materials' array
        do i=1,100
          active_mat(i)=0
        enddo

        ! Read and skip over comment lines
        do
          letter = " "
          read(myunit,'(A)') commentline
          read(commentline,*,end=976) letter
976       continue

          if (letter.ne.'#') then
            backspace(myunit)
            exit
          endif
        enddo

        ! read first parameter line
        read(myunit,*) i_ints,j_ints,k_ints
        n_mesh_cells = i_ints * j_ints * k_ints

        ALLOCATE(i_bins(1:i_ints+1))
        ALLOCATE(j_bins(1:j_ints+1))
        ALLOCATE(k_bins(1:k_ints+1))

        ! read lines 2,3,4,5
        read(myunit,*) (i_bins(i),i=1,i_ints+1)
        read(myunit,*) (j_bins(i),i=1,j_ints+1)
        read(myunit,*) (k_bins(i),i=1,k_ints+1)
        read(myunit,'(A)') line
        read(line,*,end=887) (active_mat(i),i=1,100)
887     continue

        ! counting number of activated materials specified
        do i=1,100
          if (active_mat(i).eq.0) exit
        enddo
        n_active_mat = i-1

end subroutine read_header
subroutine voxel_sample
! Sample photon position from alias table of voxels.
  use source_data

        real(dknd) :: rand

        ! Sampling the alias table
        rand = rang() * n_mesh_cells
        alias_bin = INT(rand) + 1
        if ((rand+(1._dknd-alias_bin)).lt.pairsProbabilities(alias_bin)) then
          voxel = pairs(alias_bin,1)
        else
          voxel = pairs(alias_bin,2)
        endif
        
        ! Bad condition checking; Indicates problem with alias table creation.
        if (voxel.eq.-1) then
          call expirx(1,'voxel_sample','Invalid indice sampled.')
        endif
       
        ! We -=1 the value of the index 'voxel' to calc ii,jj,kk easily
        voxel = voxel - 1
        ! Math to get mesh indices in each dimension
        ii = voxel / (k_ints*j_ints)
        jj = mod(voxel, k_ints*j_ints) / k_ints
        kk = mod(mod(voxel, k_ints*j_ints), k_ints)
        
        voxel = voxel + 1
        
        call sample_within_voxel
        
end subroutine voxel_sample


subroutine sample_within_voxel
! Samples within the extents of a voxel
! 
! ii, jj, kk are presumed to have been already determined.
  use source_data
 
!       Sample random spot within the voxel
        xxx = i_bins(ii+1)+rang()*(i_bins(ii+2)-i_bins(ii+1))
        yyy = j_bins(jj+1)+rang()*(j_bins(jj+2)-j_bins(jj+1))
        zzz = k_bins(kk+1)+rang()*(k_bins(kk+2)-k_bins(kk+1))

end subroutine sample_within_voxel


subroutine uniform_sample
! Uniformly sample photon position in the entire volume of the mesh tally.
  use source_data

        ! Choose position
        xxx = i_bins(1)+rang()*(i_bins(i_ints+1)-i_bins(1))
        yyy = j_bins(1)+rang()*(j_bins(j_ints+1)-j_bins(1))
        zzz = k_bins(1)+rang()*(k_bins(k_ints+1)-k_bins(1))

        ! Identify corresponding voxel
        do ii=1,i_ints
          if (i_bins(ii).le.xxx.and.xxx.lt.i_bins(ii+1)) exit
        enddo
        do jj=1,j_ints
          if (j_bins(jj).le.yyy.and.yyy.lt.j_bins(jj+1)) exit
        enddo
        do kk=1,k_ints
          if (k_bins(kk).le.zzz.and.zzz.lt.k_bins(kk+1)) exit
        enddo

        voxel = (kk-1)+(jj-1)*k_ints+(ii-1)*j_ints*k_ints+1

end subroutine uniform_sample
subroutine gen_voxel_alias_table
! tot_list does not have to be normalized prior to calling this subroutine
  use source_data
   
        ! Note that the first entry for each voxel in 'gammas' is
        ! a relative probability of that voxel being the source location.
        ! If biasing is used, the second entry is the bias value of the voxel
        ! Sum up a normalization factor
        sourceSum = sum(tot_list)
        write(*,*) "sourceSum:", sourceSum 

        ALLOCATE(bins(1:n_mesh_cells,1:2))
        ALLOCATE(pairs(1:n_mesh_cells, 1:2))
        ALLOCATE(pairsProbabilities(1:n_mesh_cells))

        ! make the unsorted list of bins
        bias_probability_sum = 0
        do i=1,n_mesh_cells
          ! the average bin(i,1) value assigned is n_inv
          bins(i,1) = tot_list(i) / sourceSum
          bins(i,2) = i

          ! if biasing being done, get the quantity: sum(p_i*b_i)
          !  where for bin i, p_i is bin probability, b_i is bin bias
          if (bias.eq.1) then
            bias_probability_sum = & 
                              bias_probability_sum + bins(i,1) * bias_list(i)
          endif
        enddo

        ! if bias values were found, update the bin(i,1) values for biasing
        !  and then update the bias values so that they are now particle wgt
        if (bias.eq.1) then
          do i=1,n_mesh_cells
            bins(i,1) = bins(i,1) * bias_list(i) / bias_probability_sum
            !spectrum(i,2) = bias_probability_sum / spectrum(i,2)
            ! !!! spectrum(i,2) value is now a weight, rather than a probabilty
            bias_list(i) = bias_probability_sum / bias_list(i)
            !!! bias_list(i) value is now a weight, rather than a probabilty
          enddo
        endif

        call gen_alias_table(bins, pairs, pairsProbabilities, n_mesh_cells)

        write(*,*) 'Alias table of source voxels generated!'

end subroutine gen_voxel_alias_table


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
