private
apply: entries 

	EpLogBrowserOperationFactory new
		theLog: monitor log;
		entries: entries;
		errorHandlerBlock: [:error | error signal ];
		apply.
