accessing
fontName: fontName size: fontSize
	| newTextStyle |
	newTextStyle := ((TextStyle named: fontName asSymbol) ifNil: [ TextStyle default ]) copy.
	textStyle := newTextStyle.
	text addAttribute: (TextFontChange fontNumber: (newTextStyle fontIndexOfSize: fontSize)).
	paragraph ifNotNil: [paragraph textStyle: newTextStyle]