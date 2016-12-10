test shadowing2
testTraitTempShadowing
	
	| warningCount |
	
	warningCount := 0.
	
	self initializeErrorMessage.
	
	[ 
			OpalCompiler new 
				source: self tempTraitShadowingString;
				class: ArrayTest;
				failBlock: [self fail.];
				translate.
	] 
	on: OCShadowVariableWarning 
	do: [ :ex | 
		warningCount := warningCount + 1.
		self 
			assert: ex warningMessage 
			equals: (self traitTempShadowingWarnings at: warningCount).
		
		ex resume
	].

	self assert: warningCount equals: 3