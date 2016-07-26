primitives
primManualSurface: surfaceID setPointer: pointer
	"The 'surfaceID' is a handle returned by #primitiveCreateManualSurface from SurfacePlugin. The pointer is a 32-bit unsigned integer that SurfacePlugin casts to a void*."
	<primitive: 'primitiveSetManualSurfacePointer' module: 'SqueakFFIPrims'>
	self primitiveFailed
