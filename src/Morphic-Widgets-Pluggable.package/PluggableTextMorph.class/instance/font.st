accessing
font
	"Answer the probable font"

	^self textMorph
		ifNil: [TextStyle defaultFont]
		ifNotNil: [:m | m font]