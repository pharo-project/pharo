tests - arithmetic
testArithmeticAcrossDateBoundary

	| t1 t2 |
	t1 := '2004-01-07T11:55:00+00:00' asDateAndTime. 
	t2 := t1 - ( (42900+1) seconds).  
	self assert: t2 = ('2004-01-06T23:59:59+00:00' asDateAndTime)
		
