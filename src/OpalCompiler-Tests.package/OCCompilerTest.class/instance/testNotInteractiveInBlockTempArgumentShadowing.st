test shadowing2
testNotInteractiveInBlockTempArgumentShadowing
	
	interactive := false.
	self initializeErrorMessage.
	self 
		should: [ 
			OpalCompiler new
				source: 'temp [:temp | |temp|]';
				class: MockForCompilation;
				requestor: self;
				translate.
		] 
		raise: OCShadowVariableWarning 
		withExceptionDo: [ :ex |
			self 
				assert: ex warningMessage 
				equals: 'MockForCompilation>>temp(temp is shadowed)'
		].