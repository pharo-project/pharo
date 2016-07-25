accessing class hierarchy
addSubclass: aSubclass 
	"Make the argument, aSubclass, be one of the subclasses of the receiver. 
	Create an error notification if the argument's superclass is not the receiver."
	
	self explicitRequirement