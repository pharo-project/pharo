event handling
visitWindowEvent: windowEvent
	osWindow ifNil: [ ^self ].
	
	windowEvent event = SDL_WINDOWEVENT_EXPOSED ifTrue: [
		^ ((OSWindowExposeEvent for: osWindow) timestamp: windowEvent timestamp) deliver.
	].
	windowEvent event = SDL_WINDOWEVENT_CLOSE ifTrue: [ 
		^ (OSWindowCloseEvent for: osWindow) deliver.
	].
	windowEvent event = SDL_WINDOWEVENT_MOVED ifTrue: [ 
		^ (OSWindowMoveEvent for: osWindow) deliver
	].
	windowEvent event = SDL_WINDOWEVENT_RESIZED ifTrue: [ 
		^ (OSWindowResizeEvent for: osWindow)
				width: windowEvent data1;
				height: windowEvent data2;
				deliver.
	].
	windowEvent event = SDL_WINDOWEVENT_ENTER ifTrue: [ 
		^ (OSWindowMouseEnterEvent for: osWindow) deliver
	].
	windowEvent event = SDL_WINDOWEVENT_LEAVE ifTrue: [ 
		^ (OSWindowMouseLeaveEvent for: osWindow) deliver
	].
	windowEvent event = SDL_WINDOWEVENT_FOCUS_GAINED ifTrue: [ 
		^ (OSWindowFocusInEvent for: osWindow) deliver
	].
	windowEvent event = SDL_WINDOWEVENT_FOCUS_LOST ifTrue: [ 
		^ (OSWindowFocusOutEvent for: osWindow) deliver
	].
	windowEvent event = SDL_WINDOWEVENT_MAXIMIZED ifTrue: [ 
		^ (OSWindowMaximizedEvent for: osWindow) deliver
	].
	windowEvent event = SDL_WINDOWEVENT_MINIMIZED ifTrue: [ 
		^ (OSWindowMinimizedEvent for: osWindow) deliver
	].
