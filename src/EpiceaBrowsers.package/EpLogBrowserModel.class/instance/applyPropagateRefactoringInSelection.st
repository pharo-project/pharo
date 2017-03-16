menu - operations
applyPropagateRefactoringInSelection

	| entries |
	entries := self selectedEntryItems.
	entries ifEmpty: [ ^ self inform: 'No code changes to apply' ].
	
	EpLogBrowserOperationFactory new
		theLog: log;
		entries: { entries first };  "TODO: only first?"
		errorHandlerBlock: self operationsErrorHandlerBlock;
		applyPropagateRefactoring.
