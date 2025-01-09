Extension { #name : 'OCCodeSnippetTest' }

{ #category : '*Flashback-Decompiler-Tests' }
OCCodeSnippetTest >> testDecompile [

	| method ast |
	method := snippet compile.
	method ifNil: [ ^ self skip ]. "Another test responsibility"
	ast := method decompile.
	self assert: ast isMethod.
	ast := method parseTree.
	self assert: ast isMethod.
	"Decompilation lose many information. Not sure how to test more"
]

{ #category : '*Flashback-Decompiler-Tests' }
OCCodeSnippetTest >> testDecompileIR [

	| method ir |
	method := snippet compile.
	method ifNil: [ ^ self skip ]. "Another test responsibility"
	ir := method decompileIR.
	self assert: ir class equals: OCIRMethod.
	"Decompilation lose information. Not sure how to test more"
]
