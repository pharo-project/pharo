updating screen
updateAreas: allDamage immediate: forceToScreen
	| pixels pitchHolder pitch intersection updateUnion externalForm |
	self validate
		ifFalse: [ ^ self ].
		
	pixels := ExternalAddress new.
	pitchHolder := ExternalAddress allocate: 4.
	[
		texture lockPixels: pixels pitch: pitchHolder.
		pitch := pitchHolder signedLongAt: 1.
	] ensure: [ pitchHolder free ].
	
	updateUnion := nil.
	[
		externalForm := OSSDL2ExternalForm extent: extent depth: 32 bits: pixels.
		allDamage do: [ :rectangle |
			intersection := rectangle
				intersect: (0 @ 0 corner: window extent)
				ifNone: [ nil ].
		
			intersection ifNotNil: [
				self primitiveUpdateRectangle: rectangle externalForm: externalForm.
				updateUnion ifNil: [
					updateUnion := intersection 
				] ifNotNil: [
					updateUnion := updateUnion merge: intersection
				]
			]
		].
	] ensure: [
		texture unlock.
		externalForm destroySurface
	].

	updateUnion ifNotNil: [
		self copyAndPresentTextureRectangle: updateUnion
	].

	