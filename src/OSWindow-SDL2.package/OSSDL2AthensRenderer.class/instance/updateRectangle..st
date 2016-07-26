updating screen
updateRectangle: rectangle
	texture ifNil: [ ^ self ].
	renderer clear.
	texture updateTexturePixels: pixels pitch: pitch value.
	renderer
		copy: texture srcRect: rectangle asSDLRect dstRect: rectangle asSDLRect;
		present