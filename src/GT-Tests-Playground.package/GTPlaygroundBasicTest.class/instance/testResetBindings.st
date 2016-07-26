running
testResetBindings
	|obtainedBindings|

	window := playground openOn: (GTPlayPage new saveContent: 'number+1').
	playground 
		setBindings: { #number -> 2 } asDictionary.
		
	obtainedBindings := playground currentBindings.
	self assert: obtainedBindings size equals: 1.

	playground resetBindings.		
	
	obtainedBindings := playground currentBindings.
	self assert: obtainedBindings size equals: 0.
	