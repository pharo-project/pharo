window creation and deletion
createWindowWithAttributes: attributes osWindow: osWindow
	| flags handle window glAttributes x y |
	flags := attributes visible ifTrue: [ SDL_WINDOW_SHOWN ] ifFalse: [ SDL_WINDOW_HIDDEN ].
	attributes fullscreen ifTrue: [ flags := flags | SDL_WINDOW_FULLSCREEN_DESKTOP ].
	
	attributes borderless ifTrue: [ flags := flags | SDL_WINDOW_BORDERLESS ].
	
	" Set the OpenGL attributes."
	glAttributes := attributes glAttributes.
	glAttributes ifNotNil: [ 
		flags := flags | SDL_WINDOW_OPENGL.
		self setGLAttributes: glAttributes.
	].
	
	"Get the actual initial position value."
	x := attributes x.
	y := attributes y.
	attributes windowCentered ifTrue: [
		x := SDL_WINDOWPOS_CENTERED.
		y := SDL_WINDOWPOS_CENTERED.
	].

	"Extra creation flags"
	attributes resizable ifTrue: [ 
		flags := flags | SDL_WINDOW_RESIZABLE.
	].

	attributes maximized ifTrue: [ 
		flags := flags | SDL_WINDOW_MAXIMIZED.
	].
	
	attributes minimized ifTrue: [ 
		flags := flags | SDL_WINDOW_MINIMIZED.
	].

	"Create the window"
	handle := SDL2 createWindow: attributes title
				x: x
				y: y
				width: attributes width 
				height: attributes height flags: flags.
	window := OSSDL2WindowHandle newWithHandle: handle attributes: attributes.
	window osWindow: osWindow.
	self registerWindow: window.
	^ window