"
An ExternalForm is a specialized Form whose pixel-data is stored in memory that the user provides a pointer to.  This can simply be memory on the C heap, or (the motivating use-case...) it can be a pointer that is temporarily ""mapped"" from GPU memory by an API such as OpenCL.

IMPORTANT: Moved form FFI to SDL2 because is not needed there anymore but OSWindow-SDL2 package uses it.

The user is responsible for both releasing the image-memory, as well as destroying the surface handle (perhaps the latter should be handled by automatic finalization).

Example usage:

| extent form ptr |
extent := 400@300.
form := ExternalForm extent: extent depth: 32.
ptr := ExternalAddress gcallocate: (extent x * extent y * 4).
form setManualSurfacePointer: ptr.
Display displayScaledOn: form.
form displayAt: 0@0.
form destroySurface.

"
Class {
	#name : 'OSSDL2ExternalForm',
	#superclass : 'Form',
	#instVars : [
		'pointer'
	],
	#category : 'OSWindow-SDL2-Base',
	#package : 'OSWindow-SDL2',
	#tag : 'Base'
}

{ #category : 'initialize' }
OSSDL2ExternalForm >> allocateSpace [
	"Convenient way to allocate space for the pixels.  This isn't done by default, because it is common to use a pointer obtained from elsewhere."
	| addr |
	pointer ifNotNil: [self error: 'space is already allocated'].
	addr := ExternalAddress gcallocate: width*height * depth/8. "area times bytes/pixel"
	self setManualSurfacePointer: addr
]

{ #category : 'initialize' }
OSSDL2ExternalForm >> destroySurface [
	"Users must call this explicitly when this object is no longer needed; otherwise, resource-leakage will occur in the SurfacePlugin"
	bits ifNotNil: [:surfaceID |
		bits := nil.
		self primDestroyManualSurface: surfaceID
	]
]

{ #category : 'scaling, rotation' }
OSSDL2ExternalForm >> magnifyBy: scale smoothing: cellSize [

	| form |
	
	form := Form extent: self extent depth: self depth.
	form getCanvas image: self at: 0@0 sourceRect: self boundingBox rule: 34.
	^ form magnifyBy: scale smoothing: cellSize
]

{ #category : 'accessing' }
OSSDL2ExternalForm >> pointer [
	^pointer
]

{ #category : 'primitives' }
OSSDL2ExternalForm >> primCreateManualSurfaceWidth: aWidth height: aHeight rowPitch: rowPitch depth: aDepth isMSB: isMSB [
	<primitive: 'primitiveCreateManualSurface' module: 'SurfacePlugin'>

	^ self primitiveFailed
]

{ #category : 'primitives' }
OSSDL2ExternalForm >> primDestroyManualSurface: surfaceID [
	<primitive: 'primitiveDestroyManualSurface' module: 'SurfacePlugin'>

	^ self primitiveFailed
]

{ #category : 'primitives' }
OSSDL2ExternalForm >> primManualSurface: surfaceID setPointer: aPointer [
	"The 'surfaceID' is a handle returned by #primitiveCreateManualSurface from SurfacePlugin. The pointer is a 32-bit unsigned integer that SurfacePlugin casts to a void*."
	<primitive: 'primitiveSetManualSurfacePointer' module: 'SurfacePlugin'>

	^ self primitiveFailed
]

{ #category : 'initialize' }
OSSDL2ExternalForm >> setExtent: extent depth: bitsPerPixel [
	bits ifNotNil: [self error: 'bits are already set'].
	bitsPerPixel == 32 ifFalse: [self error: 'only 32 bits for now'.  "see 'rowPitch' below"].
	width := extent x asInteger.
	width < 0 ifTrue: [width := 0].
	height := extent y asInteger.
	height < 0 ifTrue: [height := 0].
	depth := bitsPerPixel.
	bits := self
		primCreateManualSurfaceWidth: width
		height: height
		rowPitch: width * 4
		depth: bitsPerPixel
		isMSB: true
]

{ #category : 'initialize' }
OSSDL2ExternalForm >> setExtent: extent depth: bitsPerPixel bits: aPointer [
	self setExtent: extent depth: bitsPerPixel.
	self setManualSurfacePointer: aPointer
]

{ #category : 'initialize' }
OSSDL2ExternalForm >> setManualSurfacePointer: newPointer [ "ExternalStructure, ExternalAddress, or nil"
	"Set the memory-location of the image data.  It is OK to set a NULL pointer;
	 in this case, any attempt to BitBlt to or from the form will result in a primitive-failure."
	pointer := newPointer.
	pointer ifNil: [^self primManualSurface: bits setPointer: 0].
	pointer isExternalAddress
		ifFalse: ["must already be ExternalStructure, so nothing to do"]
		ifTrue: [pointer := ExternalData
							fromHandle: newPointer
							type: ExternalType void asPointerType].
	"The primitive expects an unsigned integer arg, not an ExternalAddress."
	"NOTE: it used to be acceptable for 'newPointer' to be an Integer...
	 if you get a MNU for #getHandle here, you should update your code
	 to pass in either an ExternalStructure or an ExternalAddress."
	self primManualSurface: bits setPointer: pointer getHandle asInteger
]
