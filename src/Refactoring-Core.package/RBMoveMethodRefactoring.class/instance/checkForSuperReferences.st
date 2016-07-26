preconditions
checkForSuperReferences
	| searcher |
	searcher := RBParseTreeSearcher new.
	searcher matches: 'super `@message: `@args' do: [:aNode :answer | true].
	(searcher executeTree: parseTree initialAnswer: false) 
		ifTrue: 
			[self refactoringError: 'Cannot move the method since it has a super message send.']