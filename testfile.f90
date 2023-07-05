    ! my test file
    
    module SomeModule
        implicit none
        contains
        elemental function A(x) result(res)
            integer :: res
            integer, intent(IN) :: x
            res = x + 1
        end function
    end module SomeModule
    
    program TESTFILE
    
        use SomeModule, DoSomething => A
    
        implicit none
        
        integer(4) :: ierr
        real(4) :: input
        real(8) :: sum, area
        real(8), parameter :: pi = 3.141592653589793
        
        !Declare variables
        integer, parameter :: m = 3, n = 3
        integer, pointer :: p(:)=>null(), q(:,:)
        integer, allocatable, target :: A(:,:), B(:)
        integer :: istat = 0, i, j
        character(80) :: fmt
        real(8), pointer :: k(:,:,:), k1(:), k2(:), k3(:,:), k4(:,:), kresult(:,:)

        !  Write format string for matrices
        !  (/ A / A, " = [", 3( "[",3(i2, 1x), "]" / 5x), "]" )
        write (fmt, '("(/ A / A, "" = ["", ", i0, "( ""["",", i0, "(i3, 1x), ""]"" / 5x), ""]"" )")') m, n
 
        allocate(A(m, n), q(m, n), stat = istat)
        if (istat /= 0) stop 'Error during allocation of A and q'
        
        allocate(k(10, 11, 12), stat = istat)
        allocate(k1(10), k2(10), k3(10,1), k4(1,10), kresult(10,10))
        k = 0.01d2 ! 1.0
        if (istat /= 0) stop 'Error during allocation of k'
        
        ! sum = matmul(k(:, 1, 1), k(1, 1:141, 1))
        k1 = reshape(k(:,1,1), (/size(k1)/))
        k2 = reshape(k(1,1:size(k2):1,1), (/size(k2)/))
        !sum = k1 * k2
        sum = dot_product(k1, k2)
        k3 = reshape(k2, (/size(k2),1/))
        k4 = reshape(k2, (/1,size(k2)/))
        kresult = matmul(k3(1:10, 1:1), k4(1:1, 1:10)) ! [m, n] matmul [n, k] -> [m, k]
        print *, sum
        
        k4 = k4 ** 2
        
        sum = 0
        
        !  Matrix A is:
        !  A = [[ 1  4  7 ]
        !       [ 2  5  8 ]
        !       [ 3  6  9 ]
        !       ]
        A = reshape([(i, i = 1, size(A))], shape(A))
        B = reshape(A, (/size(A)/))
        q = A

        write(*, fmt) "Matrix A is:", "A", ((A(i, j), j = 1, size(A, 2)), i = 1, size(A, 1))
 
        !  p will be associated with the first column of A
        p => A(:, 1)
 
        !  This operation on p has a direct effect on matrix A
        p = p ** 2
 
        !  This will end the association between p and the first column of A
        nullify(p)

        !  Matrix A becomes:
        !  A = [[ 1  4  7 ]
        !       [ 4  5  8 ]
        !       [ 9  6  9 ]
        !       ]
        write(*, fmt) "Matrix A becomes:", "A", ((A(i, j), j = 1, size(A, 2)), i = 1, size(A, 1))
 
        !  Perform some array operation
        q = q + A
 
        !  Matrix q becomes:
        !  q = [[ 2  8 14 ]
        !       [ 6 10 16 ]
        !       [12 12 18 ]
        !       ]
        write(*, fmt) "Matrix q becomes:", "q", ((q(i, j), j = 1, size(A, 2)), i = 1, size(A, 1))
 
        !  Use p as an ordinary array
        allocate (p(1:m*n), stat = istat)
        if (istat /= 0) stop 'Error during allocation of p'
 
        !  Perform some array operation
        p = reshape(DoSomething(A + A ** 2), shape(p))
 
        !  Array operation:
        !      p(1) = 3
        !      p(2) = 21
        !      p(3) = 91
        !      p(4) = 21
        !      p(5) = 31
        !      p(6) = 43
        !      p(7) = 57
        !      p(8) = 73
        !      p(9) = 91
        write(*, '("Array operation:" / (4x,"p(",i0,") = ",i0))') (i, p(i), i = 1, size(p))
 
        deallocate(A, p, q, stat = istat)
        if (istat /= 0) stop 'Error during deallocation'
        
        area = 2 * pi * (55.0 ** 2 + 10 * 5)
        
        print *, area
        
        open(unit=10, file="fdata.dat", status="UNKNOWN", form="FORMATTED") ! unit == fid, status == 파일 존재 체크, form == 텍스트/바이너리
        
        sum = 0
        
        do
            print *, "Add: "
            
            read (*, *, iostat = ierr) input ! * == stdio 에서 읽기 write -> 쓰기
            ! == read (*, *) input
            ! == read *, input
            
            if (input == 0 .OR. ierr /= 0) then
                print *, "Input == 0 or IERR /= 0"
                exit
            else
                sum = sum + input
            end if
            
            write (10, *) input
        end do
        
        print *, "TEST SUM= ", sum
        write (10, *) sum
        
        close (10)
    
    end program TESTFILE