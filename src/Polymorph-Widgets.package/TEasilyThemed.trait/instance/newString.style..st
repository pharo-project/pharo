controls
newString: aStringOrText style: aStyle
	"Answer a new embossed string."

	^self theme
		newStringIn: self
		label: aStringOrText
		font: self theme labelFont
		style: aStyle