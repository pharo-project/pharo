Extension { #name : #Text }

{ #category : #'*Deprecated90' }
Text >> withSqueakLineEndings [
	"Answer a new instance where all occurrences of CRLF and LF are substituted with CR."
	
	self
		deprecated: 'Should use withInternalLineEndings instead'
		on: '30 October 2020'
		in: #Pharo9
		transformWith: '`@rec withSqueakLineEndings' -> '`@rec withInternalLineEndings'.
	
	^ self withInternalLineEndings
]
