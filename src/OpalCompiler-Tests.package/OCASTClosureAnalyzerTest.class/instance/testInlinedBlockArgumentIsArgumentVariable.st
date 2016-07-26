testing - variables
testInlinedBlockArgumentIsArgumentVariable
	| ast blockNode |
	ast := (OCOpalExamples>>#exampleForInlinedBlockArgument) parseTree.
	ast doSemanticAnalysis.
	blockNode := ast body statements first arguments first.
	self assert: blockNode arguments notEmpty.
	self assert: blockNode arguments first isArgument
	