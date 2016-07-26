utilities
whichVariableNode: aParseTree inInterval: anInterval name: aName 
	| matcher block |
	matcher := RBParseTreeSearcher new.
	block := 
			[:aNode :answer | 
			(aNode intersectsInterval: anInterval) ifTrue: [aNode] ifFalse: [answer]].
	matcher
		matches: aName do: block;
		matchesArgument: aName do: block.
	^matcher executeTree: aParseTree initialAnswer: nil