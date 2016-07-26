accessing
fontName: fontName pointSize: fontSize
	| newTextStyle |
	newTextStyle := ((TextStyle named: fontName asSymbol) ifNil: [ TextStyle default ]) copy.
	newTextStyle ifNil: [self error: 'font ', fontName, ' not found.'].

	textStyle := newTextStyle.
	text addAttribute: (TextFontChange fontNumber: (newTextStyle fontIndexOfPointSize: fontSize)).
	paragraph ifNotNil: [paragraph textStyle: newTextStyle]