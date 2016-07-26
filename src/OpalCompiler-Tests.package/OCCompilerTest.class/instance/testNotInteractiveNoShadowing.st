test shadowing
testNotInteractiveNoShadowing

	interactive := false.
	self initializeErrorMessage.

	OpalCompiler new
		source: 'temp |var2|';
		class: MockForCompilation;
		requestor: self;
		failBlock: [self fail.
				^nil];
		translate.
			
	self assert: ( newTranscript contents = '').
	


