tests-events
testMouseEnterFromMorph

	morph eventHandler on: #mouseEnter send: #value to: true.
	
	self assert: ((morph mouseEnter: nil) == true)