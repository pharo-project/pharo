tests
testEncodingVarLengthSourcePointer

	| trailer newTrailer |
	
	trailer := CompiledMethodTrailer new.
	
	trailer sourcePointer: 1.
	newTrailer := trailer testEncoding.
	
	self assert: (newTrailer sourcePointer = 1).
	
	trailer sourcePointer: 16r100000000000000.
	newTrailer := trailer testEncoding.
	self assert: (newTrailer sourcePointer = 16r100000000000000).
	"the last bytecode index must be at 0"
	self assert: (newTrailer endPC = 0).
