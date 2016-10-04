updating screen
updateRectangle: aRectangle
	| intersection pitchHolder externalForm |
	self validate ifFalse: [ ^ self ].
	intersection := aRectangle
		intersect: (0 @ 0 corner: textureExtent)
		ifNone: [ ^ self ].
	
	pixels := ExternalAddress new.
	pitchHolder := ExternalAddress allocate: 4.
	[
		texture lockPixels: pixels pitch: pitchHolder.
		pitch := pitchHolder signedLongAt: 1.
	] ensure: [ pitchHolder free ].

	externalForm := ExternalForm extent: textureExtent depth: 32 bits: pixels.
	[
		self primitiveUpdateRectangle: intersection externalForm: externalForm
	] ensure: [
		texture unlock.
		externalForm destroySurface
	].
