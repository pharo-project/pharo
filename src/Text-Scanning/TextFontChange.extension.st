Extension { #name : #TextFontChange }

{ #category : #'*Text-Scanning' }
TextFontChange >> emphasizeScanner: scanner [
	"Set the font for text display"
	scanner setFont: fontNumber
]
