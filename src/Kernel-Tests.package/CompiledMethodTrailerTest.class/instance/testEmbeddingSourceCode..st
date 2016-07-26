tests
testEmbeddingSourceCode: sourceCode

	| trailer newTrailer |
	
	trailer := CompiledMethodTrailer new.
	trailer embeddedSourceCode: sourceCode.
	newTrailer := trailer testEncoding.
	
	self assert: (trailer kind == #EmbeddedSource ).
	self assert: (newTrailer sourceCode = sourceCode).

	"the last bytecode index must be at 0"
	self assert: (newTrailer endPC = 0).