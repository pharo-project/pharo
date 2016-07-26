tests
testInitialText
	self testNewSpotter.
	
	self flag: 'is this instvar still required ? we have context'.
	self assert: spotter getText isEmptyOrNil.
	self assert: spotter text isEmptyOrNil.
