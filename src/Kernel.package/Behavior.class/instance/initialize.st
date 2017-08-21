initialization
initialize
	"moved here from the class side's #new"
	super initialize.
	self superclass: Object.
	"no longer sending any messages, some of them crash the VM"
	self methodDict: self emptyMethodDictionary.
	self setFormat: Object format.
	self traitComposition: nil.
	self users: IdentitySet new.