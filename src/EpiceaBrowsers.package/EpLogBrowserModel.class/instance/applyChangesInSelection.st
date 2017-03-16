menu - operations
applyChangesInSelection

	| entries |
	entries := self selectedCodeChanges reversed.
	entries ifEmpty: [ ^ self inform: 'No code changes to apply' ].

	EpLogBrowserOperationFactory new
		theLog: log;
		entries: entries;
		errorHandlerBlock: self operationsErrorHandlerBlock;
		apply.
