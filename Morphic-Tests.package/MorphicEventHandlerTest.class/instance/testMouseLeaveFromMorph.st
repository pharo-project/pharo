tests-events
testMouseLeaveFromMorph

	morph eventHandler on: #mouseLeave send: #value to: true.
	
	self assert: ((morph mouseLeave: nil) == true)