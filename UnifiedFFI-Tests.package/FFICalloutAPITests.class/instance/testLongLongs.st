tests atomic
testLongLongs
	"Test passing and returning longlongs"
	| long1 long2 result  |
	
	"This test fails in windows, but it is expected (is because the funtion used is in 
	 another library)"
	Smalltalk os isWindows ifTrue: [ ^ self skip ].		
	
	long1 := 16r123456789012.
	long2 := (-1 << 31).
		
	result := self ffiLongLongAbs: long1.
	self assert: result equals: long1.
	
	result := self ffiLongLongAbs: long2.
	self assert: result equals: long2 abs.