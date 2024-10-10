Extension { #name : 'ASTCodeSnippetTest' }

{ #category : '*Flashback-Decompiler-Tests' }
ASTCodeSnippetTest >> testDecompile [

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
ASTCodeSnippetTest >> testDecompileIR [

	| method ir |
	method := snippet compile.
	method ifNil: [ ^ self skip ]. "Another test responsibility"
	ir := method decompileIR.
	self assert: ir class equals: IRMethod.
	"Decompilation lose information. Not sure how to test more"
]
