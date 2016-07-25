transforming
numberOfSelfSendsIn: aParseTree 
	| search |
	search := RBParseTreeSearcher new.
	search matches: self messagePattern do: [:aNode :answer | answer + 1].
	^search executeTree: aParseTree initialAnswer: 0