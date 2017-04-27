menu - operations
applyChangesIn: entries

	entries ifEmpty: [ ^ self inform: 'No code changes to apply' ].

	EpLogBrowserOperationFactory new
		logBrowserModel: self;
		entries: entries;
		errorHandlerBlock: self operationsErrorHandlerBlock;
		applyCodeChanges.
