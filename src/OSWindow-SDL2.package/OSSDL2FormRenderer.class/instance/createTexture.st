private
createTexture
	texture := renderer createTextureFormat: SDL_PIXELFORMAT_XRGB8888
						access: SDL_TEXTUREACCESS_STREAMING width: form width height: form height.
	extent := form extent