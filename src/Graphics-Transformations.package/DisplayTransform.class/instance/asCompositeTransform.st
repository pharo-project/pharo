converting
asCompositeTransform
	"Represent the receiver as a composite transformation"
	^CompositeTransform new
		globalTransform: self
		localTransform: self species identity