tests
testPortName
	| anEvent portTwo|
	portTwo := (GLMSimplePort new) name: #portTwo; value: 2.
	anEvent := GLMPortEvent
		on:
			((GLMPort new)
				name: '1';
				yourself)
		previouslyValued: portTwo.
	self assert: anEvent portName equals: '1'