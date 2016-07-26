tests
testCreation
	| anEvent portOne portTwo |
	portOne := (GLMSimplePort new) name: #portOne; value: 1.
	portTwo := (GLMSimplePort new) name: #portTwo; value: 2.
	anEvent := GLMPortEvent on: portOne previouslyValued: portTwo.
	self assert: anEvent port equals: portOne.
	self assert: anEvent oldValue equals: portTwo.
	self assert: anEvent transmissionContext isEmpty