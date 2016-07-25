accessing
platformSourceVersion
	"The use of this primitive not recommended. Not all VMs providing that"


	"Answer a string corresponding to the version of the external platform source
	code, typically written in C and managed separately for each platform. This
	invokes an optional primitive that may not be available on all virtual machines."

	"Smalltalk vm platformSourceVersion"

	<primitive: 'primitivePlatformSourceVersion'>
	self notify: 'This virtual machine does not support the optional primitive #primitivePlatformSourceVersion' translated.
	^''