test shadowing
testNotInteractiveSiblingBlocksTempShadowing

	interactive := false.
	self initializeErrorMessage.

	OpalCompiler new
		source: 'temp [:temp | ]. [:temp | ]';
		class: MockForCompilation;
		requestor: self;
		failBlock: [self fail.
						^nil];
		translate.
	self assert: ( newTranscript contents = '').



	
					

