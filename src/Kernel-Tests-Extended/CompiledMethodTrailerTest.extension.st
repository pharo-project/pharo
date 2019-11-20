Extension { #name : #CompiledMethodTrailerTest }

{ #category : #'*Kernel-Tests-Extended' }
CompiledMethodTrailerTest >> testEmbeddingCompressedSourceCode [
	| trailer newTrailer code |
	trailer := CompiledMethodTrailer new.

	code := 'foo'.
	trailer compressSourceCode: code.
	newTrailer := trailer testEncoding.

	self assert: trailer kind identicalTo: #EmbeddedSourceQCompress.
	self assert: newTrailer sourceCode equals: code.

	"the last bytecode index must be at 0"
	self assert: newTrailer endPC equals: 0.

	code := 'testEmbeddingSourceCode

	| trailer newTrailer code |
	
	trailer := CompiledMethodTrailer new.
	
	trailer sourceCode: code.
	newTrailer := trailer testEncoding.
	
	self assert: newTrailer sourceCode equals: code.'.

	trailer compressSourceCode: code.
	self assert: trailer kind identicalTo: #EmbeddedSourceZip.
	newTrailer := trailer testEncoding.

	self assert: newTrailer sourceCode equals: code.
	"the last bytecode index must be at 0"
	self assert: newTrailer endPC equals: 0
]

{ #category : #'*Kernel-Tests-Extended' }
CompiledMethodTrailerTest >> testEmbeddingSourceCode [
	
	self testEmbeddingSourceCode: 'testEmbeddingSourceCode

	| trailer newTrailer code |
	
	trailer := CompiledMethodTrailer new.
	
	trailer sourceCode: code.
	newTrailer := trailer testEncoding.
	
	self assert: newTrailer sourceCode equals: code.'.
	
	"Test a big string"
	self testEmbeddingSourceCode: (String loremIpsum: 30000).
	
	""
]

{ #category : #'*Kernel-Tests-Extended' }
CompiledMethodTrailerTest >> testEmbeddingSourceCodeBig [	
	"Test a big string"
	
	self testEmbeddingSourceCode: (String loremIpsum: 30000)
	
]
