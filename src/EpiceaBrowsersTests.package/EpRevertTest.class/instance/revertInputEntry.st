private
revertInputEntry

	EpLogBrowserOperationFactory new
		logBrowserModel: (EpLogBrowserModel newWithLog: monitor log);
		entries: { inputEntry };
		errorHandlerBlock: [:error | error signal ];
		revertCodeChanges