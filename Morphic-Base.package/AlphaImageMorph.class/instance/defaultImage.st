initialization
defaultImage
	"Answer the default image for the receiver."

	^DefaultImage ifNil: [DefaultImage := DefaultForm asFormOfDepth: 32]