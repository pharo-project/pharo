accessing
font: aFont
	| newTextStyle |
	newTextStyle := aFont textStyle copy ifNil: [ TextStyle fontArray: { aFont } ].
	textStyle := newTextStyle.
	text addAttribute: (TextFontChange fontNumber: (newTextStyle fontIndexOf: aFont)).
	paragraph ifNotNil: [paragraph textStyle: newTextStyle]