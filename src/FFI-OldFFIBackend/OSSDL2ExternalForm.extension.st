Extension { #name : #OSSDL2ExternalForm }

{ #category : #'*FFI-OldFFIBackend' }
OSSDL2ExternalForm >> oldPrimCreateManualSurfaceWidth: aWidth height: aHeight rowPitch: rowPitch depth: aDepth isMSB: isMSB [
	<primitive: 'primitiveCreateManualSurface' module: 'SqueakFFIPrims'>
	self primitiveFailed

]

{ #category : #'*FFI-OldFFIBackend' }
OSSDL2ExternalForm >> oldPrimDestroyManualSurface: surfaceID [
	<primitive: 'primitiveDestroyManualSurface' module: 'SqueakFFIPrims'>
	self primitiveFailed

]

{ #category : #'*FFI-OldFFIBackend' }
OSSDL2ExternalForm >> oldPrimManualSurface: surfaceID setPointer: aPointer [
	"The 'surfaceID' is a handle returned by #primitiveCreateManualSurface from SurfacePlugin. The pointer is a 32-bit unsigned integer that SurfacePlugin casts to a void*."
	<primitive: 'primitiveSetManualSurfacePointer' module: 'SqueakFFIPrims'>
	self primitiveFailed

]
