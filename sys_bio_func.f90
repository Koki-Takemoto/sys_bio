program bio_func
    implicit none
    real(8) :: M0,M1,P
    real(8) :: a,d,v,s,u,h,n
    integer :: i
    a = 1d0
    d = 1d0
    v = 1d0
    u = 0.1d0
    n = 4d0
    h = 0.01d0
    s = 1d0

    P = 0d0
    M0 = 0d0
    M1 = 0d0

    do i = 1,500
        M0 = d*v/s/u*P
        M1 = 1d0/(1d0 + (P/h)**n)/a
        P = P + 0.0001d0
        write(11,*) P,M0,M1
    enddo

end program bio_func