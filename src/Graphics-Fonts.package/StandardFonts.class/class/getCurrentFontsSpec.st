font accessing
getCurrentFontsSpec
	"self getCurrentFontsSpec "
	^ self systemFontAspects collect: [:aspect | | font |
		font := self perform: aspect.
		Array with: (aspect, ':') asSymbol with: font familyName with: font pointSize]