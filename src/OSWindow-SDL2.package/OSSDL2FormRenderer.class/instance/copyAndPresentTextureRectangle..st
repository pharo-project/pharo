updating screen
copyAndPresentTextureRectangle: rectangle
	| sdlRectangle |

	deferUpdates
		ifTrue: [ ^ self ].

	sdlRectangle := rectangle asSDLRect.				
	renderer copy: texture srcRect: sdlRectangle dstRect: sdlRectangle.
	renderer present
	