Extension { #name : #SystemOrganizer }

{ #category : #'*Deprecated12' }
SystemOrganizer >> categoryOfElement: behaviorName [

	self deprecated: 'Use #categoryOfBehavior: instead' transformWith: '`@rcv categoryOfElement: `@arg' -> '`@rcv categoryOfBehavior: `@arg'.
	^ self categoryOfBehavior: behaviorName
]

{ #category : #'*Deprecated12' }
SystemOrganizer >> removeElement: behaviorName [

	self deprecated: 'Use #removeBehavior: instead' transformWith: '`@rcv removeElement: `@arg' -> '`@rcv removeBehavior: `@arg'.
	^ self removeBehavior: behaviorName
]
