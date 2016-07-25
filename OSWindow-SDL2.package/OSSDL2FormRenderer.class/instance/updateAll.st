updating screen
updateAll
	self validate
		ifFalse: [ ^ self ].
	texture updateTexturePixels: form bits pitch: form width * 4.
	deferUpdates
		ifTrue: [ ^ self ].
	renderer copy: texture.
	renderer present