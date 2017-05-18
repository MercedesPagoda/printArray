subroutine PrintDblArray(Output, DblValue, Space, Expo, NX, NY, NZ, NMAX)
!*********************************************************************************
!*                                                                               *
!*		Produce the Neat Formatted Output for an Double Array               	 *
!*                                                                               *
!*********************************************************************************
	implicit none
	integer,intent(in):: Output,	Space,	Expo,		NX,      NY,      NZ,      NMAX
	real(8),intent(in):: DblValue( NMAX )
	integer,parameter::  NxPrintDefault = 15
	integer::            decimal, NXY,		x,			y,       z,       yz,     &
                       	 x1m,     x2m,    	ipr,    	npr,     ig,      ib
	real(8)::            factor,  gmax,   	value,   	dummy
	character(6)::       outbuf,  scale
	character(12)::      field
	logical::            qbreak
!=================================================================================
!	Output		:	Output stream
!	DblValue    :	Array to be printed (stored in natural order)
!	Space		:	Field indicator (0=8 characters, 1=6 characters)
!	Expo	    :	Format type: 0=F, 1=E, 2=G
!=================================================================================
	npr = NX / NxPrintDefault + 1    !NxPrintDefault grids for X axis
	NXY = NX * NY
!---------------------------------------------------------------------------------
!	Figure out scaling factor
!---------------------------------------------------------------------------------
   scale = "      "
	if (Expo == 0) then

		!Find out maximum value to be printed
		gmax = 0.0

		do z=1,NZ
		do y=1,NY
			yz = (z-1) * NXY + (y-1) * NX
			do x=1,NX
				ig = yz + x
            dummy = abs(DblValue(ig))
				if (dummy < 1.0e+15)	gmax = max(gmax,dummy)
			enddo
		enddo
		enddo

		!Set up scaling factor
		call ScaleFactor(gmax, factor, scale, decimal, Space, Expo)

	endif
!---------------------------------------------------------------------------------
!	Write heading on new page
!---------------------------------------------------------------------------------
   write(Output, '(/"        **********************************************************************")')
   write(Output, '( "        *                        Print out Double Array                      *")')
	write(Output, '(1x,a6,1x\"**********************************************************************")') scale
!---------------------------------------------------------------------------------
	do ipr=0,npr-1

		x1m = NxPrintDefault * ipr + 1
		x2m = min( NX, x1m + NxPrintDefault - 1)

		do z=1,NZ
			qbreak = (z == 1) .or. (NY > 10)
			if (qbreak) then
				write(Output, '(/"(i,  j,  k) = ",i3,\)') x1m
				do x=x1m+1,x2m
					write(Output, '(i8,\)') x
				enddo
				write(Output, '()')  !Break at line (i,j,k)
			endif
			if (qbreak .or. NY > 1) write(Output, '()')  !Except for radidal coordinate

			do y=1,NY
				yz = (z-1) * NXY + (y-1) * NX
				write(Output, '("(*,",i3,",",i3,") ",\)') y,z  

				do x=x1m,x2m
					ig = yz + x
						
               value = DblValue(ig)

               if (abs(value) >= 1.0e+15) then
                  write(Output, '("  ----- ",\)')
               else
                  if	(Expo == 0) then        !F format
                     !        12345678901
                     field = '(0p,f8.--,\)'
                     write(outbuf, '(i2)' ) decimal
                     field(8:9) = outbuf
                     write(Output,field) value * factor
                  else if (Expo == 1) then    !E Format
                     !        12345678901
                     field = '(1p,e8.01,\)'
                     write(Output,field) value
                  else if (Expo == 2) then    !G format
                     call ScaleFactor(value, factor, field, decimal, 8, Expo)
                     write(Output,field) value
                  endif
               endif
				enddo
				write(Output, '()') 
         enddo

		enddo
	enddo
!=================================================================================
end subroutine
