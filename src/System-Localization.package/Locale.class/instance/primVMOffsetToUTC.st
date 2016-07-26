system primitives
primVMOffsetToUTC
	"Returns the offset in minutes between the VM and UTC.
	If the VM does not support UTC times, this is 0.
	Also gives us backward compatibility with old VMs as the primitive will fail and we then can return 0."
	<primitive:'primitiveVMOffsetToUTC' module: 'LocalePlugin'>
	^0