initialization
initialize
	"initialize the state of the receiver"
	super initialize.
	owner := nil.
	submorphs := EmptyArray.
	bounds := self defaultBounds.
	color := self defaultColor.