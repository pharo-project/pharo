Extension { #name : 'Delay' }

{ #category : '*Deprecated14' }
Delay class >> forDuration: aDuration [
	"Return a new Delay for the given duration."

	self deprecated: 'Use #forMilliseconds: instead' transformWith: '`@rcv forDuration: `@arg' -> '`@rcv forMilliseconds: `@arg asMilliSeconds'.
	^ self forMilliseconds: aDuration asMilliSeconds
]
