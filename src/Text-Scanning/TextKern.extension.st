Extension { #name : #TextKern }

{ #category : #'*Text-Scanning' }
TextKern >> emphasizeScanner: scanner [
	"Augment (or diminish) the kerning offset for text display"
	scanner addKern: kern
]
