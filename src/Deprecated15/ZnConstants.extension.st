Extension { #name : 'ZnConstants' }

{ #category : '*Deprecated15' }
ZnConstants class >> knownHTTPMethods [
	"Deprecated in Pharo 15: this is now the #allowedMethods ZnOption,
	so that a client or a server can allow its own set."

	self
		deprecated: 'Use the #allowedMethods ZnOption instead'
		transformWith: '`@rcv knownHTTPMethods' -> '(ZnCurrentOptions at: #allowedMethods)'.
	^ ZnCurrentOptions at: #allowedMethods
]

{ #category : '*Deprecated15' }
ZnConstants class >> maximumLineLength [
	"Deprecated in Pharo 15: this is now the #maximumLineLength ZnOption,
	so that it can be raised per client, per server or globally."

	self
		deprecated: 'Use the #maximumLineLength ZnOption instead'
		transformWith: '`@rcv maximumLineLength' -> '(ZnCurrentOptions at: #maximumLineLength)'.
	^ ZnCurrentOptions at: #maximumLineLength
]
