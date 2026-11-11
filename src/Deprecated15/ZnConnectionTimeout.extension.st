Extension { #name : 'ZnConnectionTimeout' }

{ #category : '*Deprecated15' }
ZnConnectionTimeout class >> value [
	"Deprecated in Pharo 15: this is now the #socketStreamTimeout ZnOption.
	Binding this variable no longer has any effect on Zinc."

	self
		deprecated: 'Use the #socketStreamTimeout ZnOption instead'
		transformWith: '`@rcv value' -> '(ZnCurrentOptions at: #socketStreamTimeout)'.
	^ ZnCurrentOptions at: #socketStreamTimeout
]
