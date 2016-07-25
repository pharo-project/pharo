accessing
versionLabel
	"The use of this primitive not recommended. Not all VMs providing that"

	"Answer a string corresponding to the version of virtual machine. This
	represents the version level of the Smalltalk source code (interpreter
	and various plugins) that is translated to C by a CCodeGenerator,  in
	addition to the external platform source code, typically written in C and
	managed separately for each platform.
	
	This invokes an optional primitive that may not be available on all virtual
	machines. See also vmVersion, which answers a string identifying the image
	from which virtual machine sources were generated."

	"Smalltalk vm versionLabel"

	<primitive: 'primitiveVMVersion'>
	self notify: 'This virtual machine does not support the optional primitive #primitiveVMVersion' translated.
	^''