initialization
createTexture
	
	texture := renderer createTextureFormat: SDL_PIXELFORMAT_XRGB8888 access: SDL_TEXTUREACCESS_STREAMING width: window extent x height: window extent y.