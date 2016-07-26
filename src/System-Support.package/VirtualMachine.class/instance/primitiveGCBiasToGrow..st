primitives
primitiveGCBiasToGrow: arg
	"Tell the VM to grow after tenures instead of running in a tight loop where it does IGCs over and over. For some weird reason the primitive takes an integer not a bool but oh well..."
	
	<primitive: 'primitiveSetGCBiasToGrow'>
	^self primitiveFailed