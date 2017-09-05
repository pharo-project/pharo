private - initialization
initializeCallingConventions
	"These constants map onto the flags inst var of an ExternalFunction.
	 The first eight bits define the calling convention.  Attribute flags
	 occupy bits 8 on up."
	"FFIConstants initializeCallingConventions"
	FFICallTypesMask := 255.
	FFICallTypeCDecl := 0.
	FFICallTypeApi := 1.
	FFICallFlagThreaded := 256