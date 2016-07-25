accessing
typesFor: variableName in: aParseTree model: aRBSmalltalk 
	| searcher messages |
	searcher := RBParseTreeSearcher new.
	searcher matches: variableName , ' `@message: ``@args'
		do: 
			[:aNode :answer | 
			answer
				add: aNode selector;
				yourself].
	messages := searcher executeTree: aParseTree initialAnswer: Set new.
	^(self new)
		model: aRBSmalltalk;
		findTypeFor: messages