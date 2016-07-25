defaults
statusFont
	"Answer the status font to use."

	^((TextStyle named: #BitstreamVeraSansMono) ifNil: [TextStyle default])
		fontOfPointSize: 8