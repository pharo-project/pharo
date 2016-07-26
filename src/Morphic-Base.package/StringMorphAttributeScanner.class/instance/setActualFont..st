scanning
setActualFont: aFont
	"Set the value of actualFont, from a TextFontReference"

	actualFont := aFont.
	aFont textStyle ifNotNil: [ :ts | fontNumber := ts fontIndexOf: aFont ]