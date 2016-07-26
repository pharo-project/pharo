tests
testEmbeddingCompressedSourceCode

	| trailer newTrailer code |
	
	trailer := CompiledMethodTrailer new.
	
	code := 'foo'.
	trailer compressSourceCode: code.
	newTrailer := trailer testEncoding.
	
	self assert: (trailer kind == #EmbeddedSourceQCompress ).
	self assert: (newTrailer sourceCode = code).

	"the last bytecode index must be at 0"
	self assert: (newTrailer endPC = 0).

	code := 'testEmbeddingSourceCode

	| trailer newTrailer code |
	
	trailer := CompiledMethodTrailer new.
	
	trailer sourceCode: code.
	newTrailer := trailer testEncoding.
	
	self assert: (newTrailer sourceCode = code).'.

	trailer compressSourceCode: code.
	self assert: (trailer kind == #EmbeddedSourceZip ).
	newTrailer := trailer testEncoding.
	
	self assert: (newTrailer sourceCode = code).
	"the last bytecode index must be at 0"
	self assert: (newTrailer endPC = 0).
