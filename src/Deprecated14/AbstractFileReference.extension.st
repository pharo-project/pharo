Extension { #name : 'AbstractFileReference' }

{ #category : '*Deprecated14' }
AbstractFileReference >> asUrl [
	"Convert the receiver in a file:// ZnUrl.
	Only an absolute path can be represented as a file:// URL"
	self deprecated: 'Use asZnUrl instead' transformWith: '`@rcv asUrl' -> '`@rcv asZnUrl'.
	^ self asZnUrl
]
