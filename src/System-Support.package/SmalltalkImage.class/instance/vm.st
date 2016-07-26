accessing
vm
	"Answer the object to query about virtual machine."
	
	^ vm ifNil: [vm := VirtualMachine new]