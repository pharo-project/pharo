bootstrapping
bootstrap 

	PBStepStartedSignal log: 'Starting  Process.'.

	self prepareBootstrap.
	[self createImage ] on:AssertionFailure do: [ :e | e resume ].
	self initializeImage.
		
	PBStepFinishedSignal log: 'Process ended.'
	