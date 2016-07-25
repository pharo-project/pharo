accessing
icon: aForm
	| convertedIcon surface |
	aForm ifNil: [ ^self ].
	
	convertedIcon := aForm unhibernate; asFormOfDepth: 32.
	surface := SDL2 createRGBSurfaceFromPixels: convertedIcon bits
			width: convertedIcon width height: convertedIcon height
			depth: 32 pitch: convertedIcon width *4
			rmask: 16r00ff0000 
			gmask: 16r000ff00
			bmask: 16r00000ff
			amask: 16rff000000.
	handle icon: surface.
	iconSurface ifNotNil: [ SDL2 freeSurface: surface ].
	iconSurface := surface.