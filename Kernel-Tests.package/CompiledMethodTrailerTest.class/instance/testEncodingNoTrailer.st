tests
testEncodingNoTrailer

	| trailer |
	
	trailer := CompiledMethodTrailer new.
	
	"by default it should be a no-trailer, 4 byte wide so it can be used to store a sourcePointer"	
	self assert: (trailer kind == #NoTrailer ).
	self assert: (trailer size = 4).
	
	trailer := trailer testEncoding.
	
	self assert: (trailer kind == #NoTrailer ).
	self assert: (trailer size = 4).
	"the last bytecode index must be at 0"
	self assert: (trailer endPC = 0).
