tests
testEmbeddingSourceCode

	self testEmbeddingSourceCode: 'foo'.
	
	self testEmbeddingSourceCode: 'testEmbeddingSourceCode

	| trailer newTrailer code |
	
	trailer := CompiledMethodTrailer new.
	
	trailer sourceCode: code.
	newTrailer := trailer testEncoding.
	
	self assert: (newTrailer sourceCode = code).'.
	
	"Test a big string"
	self testEmbeddingSourceCode: (String loremIpsum: 30000).