initialization
initialize
	"Initialize the receiver."

	super initialize.
	self
		objectTransformBlock: [:obj | obj];
		stringTransformBlock: [:string | string]