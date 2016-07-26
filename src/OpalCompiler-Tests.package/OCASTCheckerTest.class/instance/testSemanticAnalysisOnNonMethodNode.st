testing - simple
testSemanticAnalysisOnNonMethodNode
	| ast |
	{[ 1 + 2 ].
	thisContext.
	(OCOpalExamples >> #exampleReturn1)}
		do: [ :object | 
			ast := object sourceNode.
			ast doSemanticAnalysis ].
	#('1' 'true' 'nil' '1 + 2' '^1' '1 + 2. 2 + 3' '#(1 true)' '{ #foo . 1 }' '1+2;+3')
		do: [ :source | 
			ast := RBParser parseExpression: source.
			ast doSemanticAnalysis ].
	ast := RBParser parseMethod: 'foo 1 + 2'.
	ast doSemanticAnalysis