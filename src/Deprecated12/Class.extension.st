Extension { #name : 'Class' }

{ #category : '*Deprecated12' }
Class >> basicCategory [

	self deprecated: 'Use #category now' transformWith: '`@rcv basicCategory' -> '`@rcv category'.
	^ self category
]
