test shadowing2
testNotInteractiveShadowingOfTemp

	interactive := false.
	self initializeErrorMessage.
	self 
		should: [ 
			OpalCompiler new
				source: 'temp |temp1 temp1| ';
				class: MockForCompilation;
				requestor: self;
				translate.
		] 
		raise: OCShadowVariableWarning 
		withExceptionDo: [ :ex |
			self 
				assert: ex warningMessage 
				equals: 'MockForCompilation>>temp(temp1 is shadowed)'
		].
	