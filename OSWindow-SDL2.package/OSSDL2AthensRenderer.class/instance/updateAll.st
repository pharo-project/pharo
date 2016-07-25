updating screen
updateAll
	texture ifNil: [ ^ self ].
	self checkSession.
	texture updateTexturePixels: sdlSurface pixels pitch: sdlSurface pitch.
	renderer
		clear;
		copy: texture;
		present