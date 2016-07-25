font accessing
defaultFont: aFont
	"Establish the default text font and style"

	| aStyle newDefaultStyle |
	aFont ifNil: [^ StandardFonts setFontsFromSpec: {{#defaultFont:. StrikeFont defaultFontKey. StrikeFont defaultFontSize}}].
	aStyle := aFont textStyle ifNil: [^ self].
	newDefaultStyle := aStyle copy.
	newDefaultStyle defaultFontIndex: (aStyle fontIndexOf: aFont).
	TextSharedInformation at: #DefaultTextStyle put: newDefaultStyle.
