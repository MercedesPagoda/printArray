subroutine printIntArray(Output, IntValue, NX, NY, NZ, NMAX)
!********************************************************************************
!*                                                                              *
!*  Produce the neat formatted output for an integer array                      *
!*                                                                              *
!********************************************************************************
    implicit none
    integer,intent(in)::    Output,     NX,     NY,     NZ,     NMAX
    integer,intent(in)::    IntValue( NMAX )
    integer::	            NXY,    x,      y,      z,      yz,                 &
                            x1m,    x2m,    ipr,    npr,    ig,     ib,     value
    integer,parameter::     NxPrintDefault = 15
    real(8)::	            time
    logical::	            qremark   
!================================================================================
!	Output		:	Output stream
!	IntValue    :	Array to be printed (stored in natural order)
!	QNULL		:	true if grid is null 
!================================================================================
!--------------------------------------------------------------------------------
!   Write heading
!--------------------------------------------------------------------------------
    write(Output, '("        **********************************************************************")')
    write(Output, '("        *                        Print out Integer Array                     *")')
    write(Output, '("        **********************************************************************")') 
!--------------------------------------------------------------------------------
!   NxPrintDefault grids for X axis
	npr = NX / NxPrintDefault + 1
    NXY = NX * NY
!--------------------------------------------------------------------------------
	do ipr=0,npr-1 

		x1m	= NxPrintDefault * ipr + 1
		x2m	= min( NX, x1m + NxPrintDefault - 1)

        !Z axis
		do z=1,NZ

			qremark = (z == 1) .or. (NY > 10)
			if (qremark) then
				write(Output, '(\"(i,  j,  k) = ",i3,\)') x1m
                do x=x1m+1,x2m
					write(Output, '(i8,\)') x
                enddo
				write(Output, *) 
			endif
			if (qremark .or. NY > 0) write(Output, '()') 

			!Y axis
			do y=1,NY

				yz = (z-1) * NXY + (y-1) * NX
                write(Output, '("(*,",i3,",",i3,") ",\)') y,z  

				do x=x1m,x2m

					ig = yz + x
                    value = IntValue(ig)

!					if (QNULL(ig)) then
!                       write(Output, '("  ***** ",\)')                        
!					else 
						if (abs(value) >= 1e+8) then
                            write(Output, '("  ----- ",\)')
						else
                            write(Output, '(i8,\)') value 
						endif
!					endif

				enddo
				write(Output, '()') 

			enddo
		enddo
	enddo
!================================================================================
end subroutine
