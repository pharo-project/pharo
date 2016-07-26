initialize-release
obsolete
	"Make the receiver obsolete."
	self superclass removeSubclass: self.
	self organization: nil.
	super obsolete.