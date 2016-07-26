obsolete subclasses
removeAllObsoleteSubclasses
	"Remove all the obsolete subclasses of the receiver"
	self basicObsoleteSubclasses removeKey: self ifAbsent: [].
