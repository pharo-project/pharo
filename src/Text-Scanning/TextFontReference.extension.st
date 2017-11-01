Extension { #name : #TextFontReference }

{ #category : #'*Text-Scanning' }
TextFontReference >> emphasizeScanner: scanner [
	"Set the actual font for text display"
	scanner setActualFont: font
]
