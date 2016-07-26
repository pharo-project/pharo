running
testSubstitute: anObject inPresenceOf: anotherObject

	| result |
	self analyzer when: [:x | x == anObject ] substituteBy: [:x | 424242 ].

	result := self resultOfSerializeAndMaterialize: {anObject. anotherObject}.
	
	self assert: result first equals: 424242.
	self assert: result second equals: anotherObject.
