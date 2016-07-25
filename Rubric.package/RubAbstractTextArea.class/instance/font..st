accessing text
font: aFont
	| newTextStyle |
	newTextStyle := aFont textStyle copy ifNil: [ TextStyle fontArray: { aFont } ].
	newTextStyle defaultFontIndex: (newTextStyle fontIndexOf: aFont).
	self textStyle: newTextStyle
