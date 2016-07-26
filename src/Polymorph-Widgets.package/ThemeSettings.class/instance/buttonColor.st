button
buttonColor
	"Answer the value of buttonColor"

	^buttonColor ifNil: [self windowColor]