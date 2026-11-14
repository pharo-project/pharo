Extension { #name : 'ZnNetworkingUtils' }

{ #category : '*Deprecated15' }
ZnNetworkingUtils class >> defaultSocketStreamTimeout [
	"Deprecated in Pharo 15: this is now the #socketStreamTimeout ZnOption."

	self
		deprecated: 'Use the #socketStreamTimeout ZnOption instead'
		transformWith: '`@rcv defaultSocketStreamTimeout' -> '(ZnCurrentOptions at: #socketStreamTimeout)'.
	^ ZnCurrentOptions at: #socketStreamTimeout
]

{ #category : '*Deprecated15' }
ZnNetworkingUtils class >> defaultSocketStreamTimeout: seconds [
	"Deprecated in Pharo 15: this is now the #socketStreamTimeout ZnOption."

	self deprecated: 'Set the #socketStreamTimeout ZnOption instead'.
	ZnOptions globalDefault at: #socketStreamTimeout put: seconds
]
