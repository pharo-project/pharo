controls
newString: aStringOrText font: aFont style: aStyle
	"Answer a new embossed string."

	^self theme
		newStringIn: self
		label: aStringOrText
		font: aFont
		style: aStyle