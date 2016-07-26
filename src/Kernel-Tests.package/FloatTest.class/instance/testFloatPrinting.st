tests - conversion
testFloatPrinting
	"This test shows that floats are printed exactly. The idea is too make sure that users understand that "
	
	self assert: (0.1+0.2) printString = '0.30000000000000004'.
	self assert: (-0.1-0.2) printString =  '-0.30000000000000004'.
	self assert: 240 degreesToRadians cos =  -0.5000000000000004.
  	self assert: 240 degreesToRadians cos abs = 0.5000000000000004