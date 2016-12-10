test shadowing2
testNotInteractiveInBlockTempShadowing
	
	interactive := false.
	self initializeErrorMessage.
	self 
		should: [ 
			OpalCompiler new
				source: 'temp |var2| [:temp| |var2|]';
				class: MockForCompilation;
				requestor: self;
				failBlock: [self fail];
				translate.
		] 
		raise: OCShadowVariableWarning 
		withExceptionDo: [ :ex |
			self 
				assert: ex warningMessage 
				equals: 'MockForCompilation>>temp(var2 is shadowed)'
		].

