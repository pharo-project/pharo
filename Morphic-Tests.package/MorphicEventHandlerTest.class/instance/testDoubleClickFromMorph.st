tests
testDoubleClickFromMorph

	morph eventHandler on: #doubleClick send: #value to: true.
	
	self assert: ((morph doubleClick: nil) == true)