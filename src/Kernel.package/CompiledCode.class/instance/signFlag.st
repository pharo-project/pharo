accessing
signFlag
	"Answer the sign flag bit.  The sign flag bit may be
	 used by the VM to select an alternate bytecode set."

	^ self header < 0