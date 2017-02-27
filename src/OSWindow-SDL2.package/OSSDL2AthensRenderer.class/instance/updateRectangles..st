updating screen
updateRectangles: allDamage
	| pitchHolder externalForm intersection |
	self validate
		ifFalse: [ ^ self ].
		
	pixels := ExternalAddress new.
	pitchHolder := ExternalAddress allocate: 4.
	[
		texture lockPixels: pixels pitch: pitchHolder.
		pitch := pitchHolder signedLongAt: 1.
	] ensure: [ pitchHolder free ].
	
	[
		externalForm := OSSDL2ExternalForm extent: textureExtent depth: 32 bits: pixels.
		allDamage do: [ :rectangle |
			intersection := rectangle
				intersect: (0 @ 0 corner: window extent)
				ifNone: [ nil ].
		
			intersection ifNotNil: [
				self primitiveUpdateRectangle: rectangle externalForm: externalForm.
			]
		].
	] ensure: [
		texture unlock.
		externalForm destroySurface
	].
