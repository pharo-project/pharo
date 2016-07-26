tests
testDoubleClickTimeoutFromMorph

	morph eventHandler on: #doubleClickTimeout send: #value to: true.
	
	self assert: ((morph doubleClickTimeout: nil) == true)