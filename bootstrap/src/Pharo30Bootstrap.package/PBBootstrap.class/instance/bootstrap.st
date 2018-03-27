bootstrapping
bootstrap 

	PBStepStartedSignal log: 'Starting  Process.'.

	self
		prepareBootstrap;
		createImage;
		initializeImage.
		
	PBStepFinishedSignal log: 'Process ended.'
	