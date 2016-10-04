accessing
info
	| info |
	
	info := SDL_RendererInfo externalNew autoRelease.
	self primGetRendererInfo: info.
	^ info