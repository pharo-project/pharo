Extension { #name : 'ZnConstants' }

{ #category : '*Deprecated15' }
ZnConstants class >> maximumLineLength [
	"Deprecated in Pharo 15: this is now the #maximumLineLength ZnOption,
	so that it can be raised per client, per server or globally."

	self
		deprecated: 'Use the #maximumLineLength ZnOption instead'
		transformWith: '`@rcv maximumLineLength' -> '(ZnCurrentOptions at: #maximumLineLength)'.
	^ ZnCurrentOptions at: #maximumLineLength
]
