testing
hasIcon
	"Answer whether the receiver has an icon."
	^ container hasIconBlock or: [self complexContents icon notNil]