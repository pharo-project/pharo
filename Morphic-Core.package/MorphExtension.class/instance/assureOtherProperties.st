accessing - other properties
assureOtherProperties
	"creates an otherProperties for the receiver if needed"
	otherProperties ifNil: [self initializeOtherProperties].
	^ otherProperties