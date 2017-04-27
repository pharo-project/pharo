tests
applyInputEntry

	EpLogBrowserOperationFactory new
		logBrowserModel: (EpLogBrowserModel newWithLog: monitor log);
		entries: { inputEntry };
		errorHandlerBlock: [:error | error signal ];
		applyCodeChanges
