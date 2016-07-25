initialization
textHighlightColor
	"Answer my default text highlight color."
	^self valueOfProperty: #textHighlightColor ifAbsent: [ Color red ].
