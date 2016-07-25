*System-Finalization
startUp: resuming
	resuming ifFalse: [ ^self ].
	self restartFinalizationProcess.