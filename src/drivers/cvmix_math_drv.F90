!BOP
!\newpage
! !ROUTINE: cvmix_math_driver

! !DESCRIPTION: A routine to test the math functions in cvmix_math.F90
!\\
!\\

! !INTERFACE:

Subroutine cvmix_math_driver()

! !USES:

  use cvmix_kinds_and_types, only : cvmix_r8
  use cvmix_math,            only : cvmix_math_poly_interp,                   &
                                    cvmix_math_cubic_root_find,               &
                                    cubic => CVMIX_MATH_INTERP_CUBE_SPLINE

  Implicit None

!EOP
!BOC

  real(cvmix_r8), dimension(4) :: coeffs
  real(cvmix_r8), dimension(3) :: depth, norm_depth
  real(cvmix_r8), dimension(3) :: Ri_bulk
  real(cvmix_r8)               :: root

  depth(1) = -2.05240021240362_cvmix_r8
  depth(2) = -2.05240021250362_cvmix_r8
  depth(3) = -2.05240021260362_cvmix_r8
  norm_depth = (depth - depth(1)) / (depth(3) - depth(1))

  Ri_bulk(1) = -2.739995556420065e-5_cvmix_r8
  Ri_bulk(2) = -7.835367389899912e-7_cvmix_r8
  Ri_bulk(3) = 0.340014162061937_cvmix_r8

  call cvmix_math_poly_interp(coeffs, cubic, depth(2:3), Ri_bulk(2:3),        &
                              depth(1), Ri_bulk(1))
  root = cvmix_math_cubic_root_find(coeffs,                                   &
                                     0.5_cvmix_r8*(depth(2) + depth(3)))

  print*, 'Interpolating with depth'
  print*, '----'
  print*, coeffs
  print*, 'Root when depth = ', root
  print*, ''

  call cvmix_math_poly_interp(coeffs, cubic, norm_depth(2:3), Ri_bulk(2:3),   &
                              norm_depth(1), Ri_bulk(1))
  root = -cvmix_math_cubic_root_find(coeffs,                                  &
                                0.5_cvmix_r8*(norm_depth(2) + norm_depth(3)))
  root = root*(depth(3) - depth(1)) + depth(1)

  print*, 'Interpolating with depth'
  print*, '----'
  print*, coeffs
  print*, 'Root when depth = ', root
!EOC

End Subroutine cvmix_math_driver
