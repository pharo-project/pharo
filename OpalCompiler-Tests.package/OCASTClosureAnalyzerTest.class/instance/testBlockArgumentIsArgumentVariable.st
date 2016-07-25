testing - variables
testBlockArgumentIsArgumentVariable
	| ast blockNode |
	ast := (OCOpalExamples>>#exampleForBlockArgument) parseTree.
	ast doSemanticAnalysis.
	blockNode := ast body statements first value.
	self assert: blockNode arguments notEmpty.
	self assert: blockNode arguments first isArgument
	