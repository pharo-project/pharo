accessing
scrollbarColor
	"Answer the value of scrollbarColor"

	^scrollbarColor ifNil: [self windowColor]