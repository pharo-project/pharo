gc
gcBiasToGrow: aBool
	"Tell the VM to grow after tenures instead of running in a tight loop where it does IGCs over and over. For some weird reason the primitive takes an integer not a bool but oh well..."
	
	self primitiveGCBiasToGrow: (aBool ifTrue: [1] ifFalse: [0])