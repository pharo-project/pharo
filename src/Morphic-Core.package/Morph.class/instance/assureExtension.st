accessing - extension
assureExtension
	"creates an extension for the receiver if needed"
	extension ifNil: [self initializeExtension].
	^ extension