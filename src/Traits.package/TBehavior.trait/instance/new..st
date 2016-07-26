instance creation
new: sizeRequested 
	"Answer an initialized instance of this class with the number of indexable
	variables specified by the argument, sizeRequested."

	^ (self basicNew: sizeRequested) initialize  