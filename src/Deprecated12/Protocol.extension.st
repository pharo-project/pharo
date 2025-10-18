Extension { #name : 'Protocol' }

{ #category : '*Deprecated12' }
Protocol class >> name: aString [

	self deprecated: 'Use #named: instead' transformWith: '`@rcv name: `@arg' -> '`@rcv named: `@arg'.
	^ self named: aString
]
