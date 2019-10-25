Extension { #name : #Text }

{ #category : #'*Deprecated80' }
Text >> withSqueakLineEndings [
	"Answer a new instance where all occurrences of CRLF and LF are substituted with CR."
	
	self deprecated: 'Use #withInternalLineEndings instead.' transformWith: '`@receiver withSqueakLineEndings' -> '`@receiver withInternalLineEndings'.
	^ self withInternalLineEndings
]
