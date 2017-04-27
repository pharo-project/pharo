menu - operations
revertChangesIn: entries

	entries ifEmpty: [ ^ self inform: 'No code changes to revert' ].

	EpLogBrowserOperationFactory new
		logBrowserModel: self;
		entries: entries;
		errorHandlerBlock: self operationsErrorHandlerBlock;
		revertCodeChangesInPreviewMode.
