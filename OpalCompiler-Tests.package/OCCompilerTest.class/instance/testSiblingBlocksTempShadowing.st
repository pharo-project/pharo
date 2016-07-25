test shadowing
testSiblingBlocksTempShadowing

	interactive := true.
	self initializeErrorMessage.

	OpalCompiler new
		source: 'temp [:temp | ]. [:temp | ]';
		class: MockForCompilation;
		failBlock: [self fail.
						^nil];
		translate.
				



	
					

