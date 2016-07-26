tests - optimized blocks
testExampleSimpleBlockLocalIf
	| ast  assignment var |
	ast := (OCOpalExamples>>#exampleSimpleBlockLocalIf) parseTree.
	ast doSemanticAnalysis.

	assignment := RBParseTreeSearcher treeMatching: '`var := ``@anything' in: ast. 	
	var := assignment variable binding.
	self assert: var isWrite.
	self deny: var isEscaping.