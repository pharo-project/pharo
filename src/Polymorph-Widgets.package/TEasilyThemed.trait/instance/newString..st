controls
newString: aStringOrText
	"Answer a new embossed string."

	^self theme
		newStringIn: self
		label: aStringOrText
		font: self theme labelFont
		style: #plain