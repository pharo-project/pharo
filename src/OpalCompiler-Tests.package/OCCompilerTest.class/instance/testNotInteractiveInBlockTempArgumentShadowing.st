test shadowing
testNotInteractiveInBlockTempArgumentShadowing
	
	interactive := false.
	self initializeErrorMessage.

	OpalCompiler new
		source: 'temp [:temp | |temp|]';
		class: MockForCompilation;
		requestor: self;
		translate.
	self assert: newTranscript contents equals:   '
MockForCompilation>>temp(temp is shadowed)'
