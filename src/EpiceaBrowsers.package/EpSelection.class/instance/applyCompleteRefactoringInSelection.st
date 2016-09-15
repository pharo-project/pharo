operations
applyCompleteRefactoringInSelection

	self selectedEntryItems ifNotEmpty: [ :entries | 
		| entry |
		entry := entries first entries first. 
		self flag: #todo. "FIX"
	
		self 
			trigger: [ self applyRefactoring: entry content asRBRefactoring ]
			with: (EpRedo forAll: { browser log referenceTo: entry })]