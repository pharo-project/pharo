test shadowing
testNotInteractiveInBlockTempInstanceVariableShadowing
	
	interactive := false.
	self initializeErrorMessage.

	OpalCompiler new
		source: 'temp [:temp | |var1|]';
		class: MockForCompilation;
		requestor: self;
		failBlock: [self fail.
				^nil];
		translate.

	self assert: ( newTranscript contents = '
MockForCompilation>>temp(var1 is shadowed)').
	