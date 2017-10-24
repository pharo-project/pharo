tests - valid context
testDebuggerIsAValidContext
	| debugger |
	
	debugger := Smalltalk tools debugger new.
	self assert: (SugsAbstractContext contextFor: debugger) class equals: SugsDebuggerContext .