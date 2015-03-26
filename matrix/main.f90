!  main.f90 
!
!  FUNCTIONS:
!  matrix - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: main
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program main
    
        use matrix_mod

        implicit none
        
        real(dp)				:: a, b(3,3),c(3,3)
		real(dp), allocatable	:: d(:,:)
        integer					:: n
		
        print *, '*********Matrix Functions*********'
        
        !transpose a matrix
        b=reshape((/1.,1.,0.,0.,2.,2.,1.,2.,3./),(/3,3/))
        call print_matrix(b)
        c=trans(b)
        call print_matrix(c)
        
        
        ! create identity matrix
		n=4
		allocate(real(dp) :: d(n,n))
        d=eye(n)
        call print_matrix(d)
        
		
        read(*,*)
    end program main

