tests
testAbs
	self assert: aDuration abs = aDuration. 
	self assert: (Duration nanoSeconds: -5)  abs =  (Duration nanoSeconds: 5). 
