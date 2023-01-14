program sys_bio
    implicit none
    real(8) :: M,R,Q,P,dM,dR,dQ,dP
    real(8) :: a,b,c,d,v,u,h,n,s
    real(8) :: t,dt
    integer :: i

    ! 初期値
    dt = 0.01d0
    a = 1d0
    b = 1d0
    d = 1d0
    v = 1d0
    u = 0.1d0
    c = u
    ! n = 10d0
    write(*,*) "Enter n >"
    read(*,*) n
    h = 0.01d0
    s = 1d0

    M = 0.5d0
    R = 0.5d0
    Q = 0.5d0
    P = 0.5d0

    open(11,file="result.dat")
    do i = 0,10000
        write(11,*) t,M,R,Q,P

        dM = ((1d0 + (P/h)**n)**(-1d0) - a*M)*dt
        dR = (s*M - b*R + c*Q)*dt
        dQ = (b*R - (c+d+u)*Q + v*P)*dt
        dP = (u*Q - v*P)*dt

        t = t + dt
        M = M + dM
        R = R + dR
        Q = Q + dQ
        P = P + dP

    enddo
    close(11)


end program sys_bio