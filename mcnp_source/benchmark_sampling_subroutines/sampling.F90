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
