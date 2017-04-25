updating screen
updateRectangle: rectangle
	| intersection pixels pitch pitchHolder externalForm |
	intersection := rectangle
		intersect: (0 @ 0 corner: window extent)
		ifNone: [ ^ self ].
	self validate
		ifFalse: [ ^ self ].
		
	pixels := ExternalAddress new.
	pitchHolder := ExternalAddress allocate: 4.
	[
		texture lockPixels: pixels pitch: pitchHolder.
		pitch := pitchHolder signedLongAt: 1.
	] ensure: [ pitchHolder free ].
	
	[
		externalForm := OSSDL2ExternalForm extent: extent depth: 32 bits: pixels.
		self primitiveUpdateRectangle: intersection externalForm: externalForm
	] ensure: [
		texture unlock.
		externalForm destroySurface
	].

	self copyAndPresentTextureRectangle: intersection.
