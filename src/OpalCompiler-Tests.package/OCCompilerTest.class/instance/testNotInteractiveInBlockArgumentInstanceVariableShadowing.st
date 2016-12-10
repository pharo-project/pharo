test shadowing2
testNotInteractiveInBlockArgumentInstanceVariableShadowing

	interactive := false.
	self initializeErrorMessage.

	self 
		should: [ 
			OpalCompiler new 
				source: 'temp [:var1 | ]';
				class: MockForCompilation;
				translate.
		] 
		raise: OCShadowVariableWarning 
		withExceptionDo: [ :ex |
			self 
				assert: ex warningMessage 
				equals: 'MockForCompilation>>temp(var1 is shadowed)'
		].
