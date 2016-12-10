test shadowing2
testNotInteractiveSiblingBlocksTempShadowing

	interactive := false.
	self initializeErrorMessage.

	self shouldnt: [ 
		OpalCompiler new
			source: 'temp [:temp | ]. [:temp | ]';
			class: MockForCompilation;
			requestor: self;
			failBlock: [self fail. ^nil];
			translate
	] raise: OCShadowVariableWarning.




	
					

