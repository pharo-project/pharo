examples
simpleWindow
	"
	self new simpleWindow
	"
	| window |
	SDL2 initVideo.
	window := SDL2 createWindow: 'Test Window'
					x: SDL_WINDOWPOS_UNDEFINED
					y: SDL_WINDOWPOS_UNDEFINED
					width: 640
					height: 480
					flags: SDL_WINDOW_SHOWN.
	SDL2 delay: 3000.
	window destroy.