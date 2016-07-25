transforming
placeholderNode
	| node |
	node := RBParseTreeSearcher treeMatching: self methodDelimiter
				in: modifiedParseTree.
	node isNil ifTrue: [self refactoringFailure: 'Cannot extract code'].
	^node