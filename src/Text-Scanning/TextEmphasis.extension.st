Extension { #name : #TextEmphasis }

{ #category : #'*Text-Scanning' }
TextEmphasis >> emphasizeScanner: scanner [
	"Set the emphasis for text scanning"
	scanner addEmphasis: emphasisCode
]
