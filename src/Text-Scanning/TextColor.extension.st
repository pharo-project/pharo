Extension { #name : #TextColor }

{ #category : #'*Text-Scanning' }
TextColor >> emphasizeScanner: scanner [
	"Set the emphasis for text display"
	scanner textColor: color
]
