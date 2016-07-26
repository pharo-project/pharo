tests
testEncodingZeroSourcePointer

	| trailer |
	
	trailer := CompiledMethodTrailer new.

	self assert: 
		(trailer sourcePointer: 0) testEncoding sourcePointer = 0
	