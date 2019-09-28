Extension { #name : #TextAction }

{ #category : #'*Text-Scanning' }
TextAction >> emphasizeScanner: scanner [
	"Set the emphasis for text display"
	scanner textColor: Smalltalk ui theme urlColor
]
