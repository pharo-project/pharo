gc
gcBiasToGrowLimit: arg
	"Tell the VM the grow limit if the GC logic has bias to grow."
	
	<primitive: 'primitiveSetGCBiasToGrowGCLimit'>
	^self primitiveFailed