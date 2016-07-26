tests
testCreationWithTransmissionContext
	| anEvent aContext portOne portTwo |
	aContext := OrderedCollection new.
	portOne := (GLMSimplePort new) name: #portOne; value: 1.
	portTwo := (GLMSimplePort new) name: #portTwo; value: 2.
	anEvent := GLMPortEvent on: portOne previouslyValued: portTwo in: aContext.
	self assert: anEvent port equals: portOne.
	self assert: anEvent oldValue equals: portTwo.
	self assert: anEvent transmissionContext equals: aContext