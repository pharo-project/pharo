test shadowing
testSiblingBlocksInstanceVariableShadowing

	interactive := true.
	self initializeErrorMessage.
	
	OpalCompiler new
		source: 'temp [:temp | ].[:temp | |var1|]';
		class: MockForCompilation;
		requestor: self;
		failBlock: [
				self assert: (errorMessage = 'Name already defined ->').
				self assert: (errorLocation = 27).
				^nil];
		translate.
	self fail.


	