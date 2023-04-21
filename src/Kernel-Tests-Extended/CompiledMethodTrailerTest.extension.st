Extension { #name : #CompiledMethodTrailerTest }

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

	"Test an empty DoIt"
	self testEmbeddingSourceCode: ''
]

{ #category : #'*Kernel-Tests-Extended' }
CompiledMethodTrailerTest >> testEmbeddingSourceCodeBig [
	"Test a big string"

	self testEmbeddingSourceCode: (String loremIpsum: 30000)
]
