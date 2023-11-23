Extension { #name : 'Announcement' }

{ #category : '*Deprecated12' }
Announcement >> affectsMethodsTaggedWith: protocol [

	self deprecated: 'Use #affectsMethodsInProtocol: instead.' transformWith: '`@rcv affectsMethodsTaggedWith: `@arg' -> '`@rcv affectsMethodsInProtocol: `@arg'.
	^ self affectsMethodsInProtocol: protocol
]
