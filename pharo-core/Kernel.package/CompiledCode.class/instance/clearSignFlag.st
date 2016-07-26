accessing
clearSignFlag
	"Clear the sign flag bit.  The sign flag bit may be
	 used by the VM to select an alternate bytecode set."

	self signFlag ifTrue:
		[self objectAt: 1 put: self header - SmallInteger minVal]