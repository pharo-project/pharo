cleaning
voidCogVMState
	"Tell the VM to remove all references to any machine code form of the method.
	 This primitive must be called whenever a method is in use and modified.  This is
	 more aggressive (and *much* more costly) than flushCache since it must search
	 through all context objects, making sure that none have a (hidden) machine code pc
	 in the receiver.  Since modifying a method will likely change the generated machine code,
	 modifying a method (rather than redefining it) requires this more aggressive flush."

	<primitive: 215>
	^self flushCache