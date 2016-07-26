test shadowing
testNotInteractiveInBlockArgumentInstanceVariableShadowing

	interactive := false.
	self initializeErrorMessage.

	OpalCompiler new 
		source: 'temp [:var1 | ]';
		class: MockForCompilation;
		translate.
	
		self assert: ( newTranscript contents = '
MockForCompilation>>temp(var1 is shadowed)').
