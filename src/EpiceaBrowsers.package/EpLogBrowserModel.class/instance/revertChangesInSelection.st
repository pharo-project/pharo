menu - operations
revertChangesInSelection

	| entries |
	entries := self selectedCodeChanges.
	entries ifEmpty: [ ^ self inform: 'No code changes to revert' ].

	EpLogBrowserOperationFactory new
		theLog: log;
		entries: entries;
		errorHandlerBlock: self operationsErrorHandlerBlock;
		revert.
