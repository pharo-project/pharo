Extension { #name : #TextAlignment }

{ #category : #'*Text-Scanning' }
TextAlignment >> emphasizeScanner: scanner [
	"Set the emphasist for text scanning"
	scanner setAlignment: alignment.
]
