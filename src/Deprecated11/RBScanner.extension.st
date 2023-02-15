Extension { #name : #RBScanner }

{ #category : #'*Deprecated11' }
RBScanner class >> scanTokens: aStringOrStream [
	"Return the tokens (and not the scanner token objects)."
	self deprecated: 'Unused in standard image Pharo 11, therefore deprecated without replacement'.
	
	^ (self scanTokenObjects: aStringOrStream) collect: [ :t | t value ]
]
