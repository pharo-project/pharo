examples
simpleBlackWindow
	"
	self new simpleBlackWindow
	"
	| window renderer |
	SDL2 initVideo.
	window := SDL2 createWindow: 'Test Window'
					x: SDL_WINDOWPOS_UNDEFINED
					y: SDL_WINDOWPOS_UNDEFINED
					width: 640
					height: 480
					flags: SDL_WINDOW_SHOWN.
	renderer := window createDefaultRenderer.
	renderer drawColorR: 0 g: 0 b: 0 a: 0;
			  clear;
			  present.
	SDL2 delay: 3000.
	renderer destroy.
	window destroy.