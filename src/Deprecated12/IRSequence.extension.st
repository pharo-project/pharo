Extension { #name : 'IRSequence' }

{ #category : '*Deprecated12' }
IRSequence >> notEmpty [
	self deprecated: 'Use #isNotEmpty instead.' transformWith: '`@rcv notEmpty' -> '`@rcv isNotEmpty'.
	^self isNotEmpty
]
