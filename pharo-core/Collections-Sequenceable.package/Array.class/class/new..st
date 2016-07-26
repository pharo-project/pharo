instance creation
new: sizeRequested 
	"Answer an instance of this class with the number of indexable
	variables specified by the argument, sizeRequested.
	
	This is a shortcut (direct call of primitive, no #initialize, for performance"

	<primitive: 71>  "This method runs primitively if successful"
	^ self basicNew: sizeRequested  "Exceptional conditions will be handled in basicNew:"
