! @Name     Matrix Module
!           Functions useful for matrix manupulations
!    
! @Author Daniel Ayewah
!
!
module matrix_mod

    implicit none
    
    integer,parameter :: dp = selected_real_kind(p=15,r=307)
    
    contains
    
    real(dp) function add(x,y)
        real(dp),intent(in) :: x,y
        
        add=x+y
        
    end function add
    
    function trans(x) result(r)
        ! Transposes the matrix x
        real(dp), intent(in)    :: x(:,:)
        real(dp)                :: r(size(x, 2), size(x, 1))
        integer                 :: i,j,n, m
        
        n=size(x,1)
        m=size(x,2)
        
        do i=1,n
            do j=1,m
              r(j,i)=x(i,j)
            end do
        end do
        
    end function trans
    
    subroutine print_matrix(mat)
        ! Prints a matrix row by row
        real(dp), intent(in)    :: mat(:,:)
        integer                 :: i,n,m
        !character(len=*)        :: o_format, fmt
        
        !if(.NOT. present(o_format)) fmt='(<m>f8.2)'
        
        n=size(mat,1)
        m=size(mat,2)
        
        write(*,*)
        
        do i=1,n
            write(*,'(<m>f8.2)') mat(i,:)
        end do
        
        
    end subroutine print_matrix

end module matrix_mod

