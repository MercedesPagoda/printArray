program call_PrintArray
!********************************************************************************
!*  call_printArray                                                             *
!********************************************************************************
    implicit none
    integer,parameter::     output = 6
    integer,parameter::     NMAX = 100       
    integer::               NX,  NY,   NZ,   NXY,     x,    y,    z,    ig
    integer::               Space = 8
    integer::               Expo  = 0
    integer::               IntValue(NMAX) 
    real(8)::               DblValue(NMAX)                 
    character(1)::          key
!================================================================================
!	Space	:	Field indicator (0=8 characters, 1=6 characters)
!	Expo	:	1=Exponential format, 2=G format
!================================================================================
    do
        print '(/" Input NX, NY, NZ = ",$)' ;  read (*,*) NX, NY, NZ
        if (NX*NY*NZ > NMAX) then
            print '(" NX*NY*NZ is greater than NMAX. Input again!")'
        else
            exit
        endif
    enddo
    
100 continue
    NXY = NX * NY
    do z=1,NZ
    do y=1,NY
    do x=1,NX
        ig = NXY * (z-1) + NX * (y-1) + x 
!       write(*, '(" Input integer value at (",i4,",",i4,",",i4,") = "\)') x,y,z ; read(*,*) IntValue(ig)                     
        write(*, '(" Input double  value at (",i4,",",i4,",",i4,") = "\)') x,y,z ; read(*,*) DblValue(ig)                     
    enddo
    enddo
    enddo
    
    print '(/" Input Format type (F=0, E=1, G=2) ",$)' ;  read (*,*) Expo
    
!   call printIntArray(Output, IntValue, NX, NY, NZ, NMAX)
    call printDblArray(Output, DblValue, Space, Expo, NX, NY, NZ, NMAX)
    
    print '()'
    print '(/" Re-input value? (y/n) = ",$)' ;  read (*,*) key
    if (key == "y" .or. key == "Y") then
        goto 100
    else
        print '("Reached end of program")'
    endif
!================================================================================
    stop
end program
