examples
simpleDrawWindow
	"
	self new simpleDrawWindow
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
	SDL2 delay: 1000.
	renderer drawColorR: 0 g: 0 b: 0 a: 0;
			  clear;
			  drawColorR: 0 g: 255 b: 0 a: 255;
			  drawPointX: 50 y: 50;  
			  present.
	SDL2 delay: 1000.
	renderer
			  drawColorR: 255 g: 0 b: 0 a: 255;
			  drawLineX1: 30 y1: 30 x2: 500 y2: 30;
			  present.
	SDL2 delay: 3000.
	renderer destroy.
	window destroy.