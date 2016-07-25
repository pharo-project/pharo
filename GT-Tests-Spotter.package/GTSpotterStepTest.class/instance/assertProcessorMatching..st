convenience
assertProcessorMatching: aBlock
	| processors |
	processors := spotter currentStep processors select: [ :each | aBlock value: each ].
	
	self assert: processors notEmpty.
	self assert: processors size = 1.
	
	currentProcessor := processors anyOne.
	self assert: currentProcessor notNil.
	
	^ currentProcessor