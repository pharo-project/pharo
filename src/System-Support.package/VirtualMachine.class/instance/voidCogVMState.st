gc
voidCogVMState
	"Void any internal caches the VM maintains other than the method lookup caches.
	 These comprise
		- the stack zone, where method activations are stored, and
		- the machine code zone, where the machine code form of CompiledMethods is held."
	<primitive: 214>
	^self primitiveFailed