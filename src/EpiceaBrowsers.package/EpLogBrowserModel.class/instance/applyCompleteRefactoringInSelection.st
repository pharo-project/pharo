menu - operations
applyCompleteRefactoringInSelection

	| entries |
	entries := self selectedEntryItems.
	entries ifEmpty: [ ^ self inform: 'No code changes to apply' ].
	
	EpLogBrowserOperationFactory new
		logBrowserModel: self;
		entries: { entries first };  "TODO: only first?"
		errorHandlerBlock: self operationsErrorHandlerBlock;	
		applyCompleteRefactoring.
