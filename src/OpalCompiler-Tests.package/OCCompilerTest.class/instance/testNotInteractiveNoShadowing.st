test shadowing2
testNotInteractiveNoShadowing

	interactive := false.
	self initializeErrorMessage.

	self shouldnt: [ 
		OpalCompiler new
			source: 'temp |var2|';
			class: MockForCompilation;
			requestor: self;
			failBlock: [self fail. ^ nil ];
			translate.
	] raise: OCShadowVariableWarning.
	


