test shadowing2
testNotInteractiveSiblingBlocksInstanceVariableShadowing

	interactive := false.
	self initializeErrorMessage.
	self 
		should: [ 
			OpalCompiler new
				source: 'temp [:temp | ].[:temp | |var1|]';
				class:  MockForCompilation;
				requestor: self; 
				failBlock: [self fail. ^ nil ];
				translate.
		] 
		raise: OCShadowVariableWarning 
		withExceptionDo: [ :ex |
			self 
				assert: ex warningMessage 
				equals: 'MockForCompilation>>temp(var1 is shadowed)'
		].


	