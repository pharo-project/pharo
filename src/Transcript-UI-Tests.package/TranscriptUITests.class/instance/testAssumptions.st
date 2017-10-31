tests
testAssumptions

	self 
	assert: (Set new allSatisfy: [ :each | each value: self ]) == true;
	assert: (UIManager withAllSubclasses anySatisfy: [ :each | each canPerform: #uiProcess ]).