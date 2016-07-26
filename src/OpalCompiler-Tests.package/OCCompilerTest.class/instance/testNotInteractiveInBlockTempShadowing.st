test shadowing
testNotInteractiveInBlockTempShadowing
	
	interactive := false.
	self initializeErrorMessage.

	OpalCompiler new
		source: 'temp |var2| [:temp| |var2|]';
		class: MockForCompilation;
		requestor: self;
		failBlock: [self fail];
		translate.
	
	self assert: newTranscript contents equals:  '
MockForCompilation>>temp(var2 is shadowed)'

