examples
simpleDrawDisplay
	"
	self new simpleDrawDisplay
	"
	| window renderer texture   |
	
	"Create the window and the renderer."
	SDL2 initVideo.
	window := SDL2 createWindow: 'Test Window'
					x: SDL_WINDOWPOS_UNDEFINED
					y: SDL_WINDOWPOS_UNDEFINED
					width: 1024
					height: 480
					flags: SDL_WINDOW_SHOWN.
	renderer := window createDefaultRenderer.
	
	"Create the texture"
	texture := renderer createTextureFormat: SDL_PIXELFORMAT_XRGB8888
						access: SDL_TEXTUREACCESS_STREAMING width: Display width height: Display height.
	texture updateTexturePixels: Display bits pitch: Display width*4.
	
	"Render"
	renderer copy: texture;
			  present.
	SDL2 delay: 3000.
	
	"Quit"
	texture destroy.
	renderer destroy.
	window destroy.