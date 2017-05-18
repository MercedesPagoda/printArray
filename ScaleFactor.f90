subroutine ScaleFactor(Gmax, Factor, Form, Decimal, Space, Expo)
!*********************************************************************************
!*                                                                               *
!*		Find out a Scaling Factor and Format Statement to be Printed           	 *
!*                                                                               *
!*********************************************************************************
	implicit none
	integer,intent(in)::          Space,   Expo
	integer,intent(out)::	      Decimal
	real(8),intent(in)::          Gmax
	real(8),intent(out)::         Factor
	character(*),intent(inout)::  Form
	real(8):: 					  sgmax
!=================================================================================
!	Gmax		:  	input    :   Real value to be printed
!	Factor		:	output   :   Scaling factor
!  	Form		:  	inout    :   Format for real values to be printed   
!	Decimal		:	output   :   # of decimals places to be printed
!	Space		: 	input    :   Field indicator (8 charaters or)
!  	Expo     	:	input    :   Format type: F=0, E=1, G=2
!=================================================================================
	if (Space == 8 .and. Expo == 2) then

		if		(Gmax >= 99999.95) then		!1.0e+5) then
			Form    = '(1p,e8.1\)'
			Decimal = 1
		else if (Gmax >= 9999.995) then  !1.0e+4) then
			Form    = '(f8.0\)'
		   Decimal = 0    
		else if (Gmax >= 999.9995) then  !1.0e+3) then
			Form    = '(f8.1\)'
		   Decimal = 1
		else if (Gmax >= 99.99995) then  !1.0e+2) then
			Form    = '(f8.2\)'
		   Decimal = 2
		else if (Gmax >= 9.999995) then  !1.0e+1) then
			Form    = '(f8.3\)'
		   Decimal = 3
		else if (Gmax >= .9999995) then  !1.0e+0) then
			Form    = '(f8.4\)'
		   Decimal = 4
		else if (Gmax >= .09999995) then	!1.0e-1) then
			Form    = '(f8.5\)'
		   Decimal = 5
		else if (Gmax == 0.0) then
			Form    = '(f8.5\)'
		   Decimal = 5
		else
			Form    = '(1p,e8.1\)'
		   Decimal = 2
      endif

	else if (Space == 8 .and. Expo == 0) then

		!set scaling factor
		if (Gmax > 1.0e+3) then
			Form   = "*10**3"
			Factor = 1.0e-3
		else
			Form   = "      "
			Factor = 1.0
		endif

		!set decimal
		sgmax = Gmax * Factor
		
		if	(sgmax >= 999.9995) then
			Decimal = 0
		else if (sgmax >= 99.99995) then
			Decimal = 1
		else if (sgmax >= 9.999995) then
			Decimal = 2
		else if (sgmax >= 0.9999995) then
			Decimal = 3
		else
			Decimal = 4
      endif

	endif
!=================================================================================
end subroutine
