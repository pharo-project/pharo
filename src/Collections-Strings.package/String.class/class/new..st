instance creation
new: sizeRequested 
	"Answer an instance of this class with the number of indexable
	variables specified by the argument, sizeRequested."
	
	self == String 
		ifTrue:[^ByteString new: sizeRequested]
		ifFalse:[^self basicNew: sizeRequested].