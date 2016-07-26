accessing
setSignFlag
	"Set the sign flag bit.  The sign flag bit may be
	 used by the VM to select an alternate bytecode set."

	self signFlag ifFalse:
		[self objectAt: 1 put: self header + SmallInteger minVal]