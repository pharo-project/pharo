testing
hasOnlyOneAssignment
	| searcher |
	searcher := RBParseTreeSearcher new.
	searcher matches: assignmentNode variable name , ' := ``@object'
		do: [:aNode :answer | answer + 1].
	^(searcher executeTree: definingNode initialAnswer: 0) == 1