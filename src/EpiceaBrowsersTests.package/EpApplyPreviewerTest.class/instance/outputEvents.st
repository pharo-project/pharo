resources
outputEvents

	| outputLog logBrowserModel |
	logBrowserModel := EpLogBrowserModel newWithLog: monitor log.
	
	outputLog :=
		EpLogBrowserOperationFactory new
			logBrowserModel: logBrowserModel;
			entries: { inputEntry };
			errorHandlerBlock: [:error | error signal ];
			newApplyPreviewLog.
	
	^ outputLog entries collect: #content