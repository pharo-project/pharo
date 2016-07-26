test shadowing
testNotInteractiveShadowingOfTemp

	interactive := false.
	self initializeErrorMessage.

	OpalCompiler new 
		source: 'temp |temp1 temp1| ';
		class: MockForCompilation;
		requestor: self;
		translate.
			
	self assert: ( newTranscript contents =  '
MockForCompilation>>temp(temp1 is shadowed)').
	