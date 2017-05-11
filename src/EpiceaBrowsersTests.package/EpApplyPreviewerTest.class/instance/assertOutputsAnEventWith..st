resources
assertOutputsAnEventWith: aBlock

	| outputEvents |
	outputEvents := self outputEvents.

	self assert: outputEvents size equals: 1.
	
	aBlock value: outputEvents anyOne.
