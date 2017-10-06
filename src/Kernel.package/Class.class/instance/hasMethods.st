testing
hasMethods
	"Answer a Boolean according to whether any methods are defined for the 
	receiver (includes whether there are methods defined in the receiver's 
	metaclass)."

	^super hasMethods or: [ self theMetaClass hasMethods ]