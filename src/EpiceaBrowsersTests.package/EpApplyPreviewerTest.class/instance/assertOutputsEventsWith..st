resources
assertOutputsEventsWith: aBlock

	| logBrowserModel outputLog |
	logBrowserModel := EpLogBrowserModel newWithLog: monitor log.
	
	outputLog :=
		EpLogBrowserOperationFactory new
			logBrowserModel: logBrowserModel;
			entries: inputEntries;
			errorHandlerBlock: [:error | error signal ];
			newApplyPreviewLog.
	
	aBlock value: (outputLog entries collect: #content).
