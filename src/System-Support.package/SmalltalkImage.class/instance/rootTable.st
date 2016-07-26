memory space
rootTable
	"Primitive. Answer a snapshot of the VMs root table. 
	Keep in mind that the primitive may itself cause GC."
	<primitive: 'primitiveRootTable'>
	^self primitiveFailed