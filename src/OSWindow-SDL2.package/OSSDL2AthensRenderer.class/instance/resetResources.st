updating resources
resetResources
	| extent |
	self checkSession.
	extent := self window extent.
	self createSDLSurfaceWithExtent: extent.
	athensSurface := AthensCairoSDLSurface fromSDLSurface: sdlSurface.
	texture := renderer 
		createTextureFormat: SDL_PIXELFORMAT_XRGB8888 
		access: SDL_TEXTUREACCESS_STREAMING 
		width: extent x 
		height: extent y.
	textureExtent := extent.