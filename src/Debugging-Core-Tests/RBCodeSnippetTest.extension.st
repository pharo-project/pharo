Extension { #name : 'RBCodeSnippetTest' }

{ #category : '*Debugging-Core-Tests' }
RBCodeSnippetTest >> testDecompileIR [

	| method ir |
	method := snippet compile.
	method ifNil: [ ^ self skip ]. "Another test responsibility"
	ir := method decompileIR.
	self assert: ir class equals: IRMethod.
	"Decompilation lose information. Not sure how to test more"
]
