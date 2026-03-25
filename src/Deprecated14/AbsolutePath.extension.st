Extension { #name : 'AbsolutePath' }

{ #category : '*Deprecated14' }
AbsolutePath >> asUrl [
	"Convert the receiver in a file:// ZnUrl"
	self deprecated: 'Use asZnUrl instead' transformWith: '`@rcv asUrl' -> '`@rcv asZnUrl'.
	^ self asZnUrl
]
