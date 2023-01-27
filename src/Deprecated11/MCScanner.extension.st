Extension { #name : #MCScanner }

{ #category : #'*Deprecated11' }
MCScanner class >> scanTokens: aString [
	"compatibility"
	self deprecated: 'Unused in standard image Pharo 11, therefore deprecated without replacement'.
	
	^ Array with: (self scan: aString readStream)
]
