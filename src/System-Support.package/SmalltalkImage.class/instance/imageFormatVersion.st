image
imageFormatVersion
	"Answer an integer identifying the type of image. The image version number may
	identify the format of the image (e.g. 32 or 64-bit word size) or specific requirements
	of the image (e.g. block closure support required). This invokes an optional primitive
	that may not be available on all virtual machines."

	"Smalltalk image imageFormatVersion"

	<primitive: 'primitiveImageFormatVersion'>
	self notify: 'This virtual machine does not support the optional primitive #primitiveImageFormatVersion' translated.
	^''