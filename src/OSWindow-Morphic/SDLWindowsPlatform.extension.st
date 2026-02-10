Extension { #name : 'SDLWindowsPlatform' }

{ #category : '*OSWindow-Morphic' }
SDLWindowsPlatform >> afterMainPharoWindowCreated: aOSSDLWindow [

	| sdlWindowInfo handle |
	sdlWindowInfo := aOSSDLWindow backendWindow getWMInfo.
	handle := sdlWindowInfo info win window.
	self enableDebugWindowMenu: handle. 

	self addRestartUIManagerOption: handle.

	"I want to receive the System Windows events"
	SDL2 eventType: SDL_SYSWMEVENT state: 1.
	
	self wtsRegisterSessionNotification: handle.

]

{ #category : '*OSWindow-Morphic' }
SDLWindowsPlatform >> beforeMainPharoWindowClosed: aOSSDLWindow [

	| sdlWindowInfo handle |
	sdlWindowInfo := aOSSDLWindow backendWindow getWMInfo.
	handle := sdlWindowInfo info win window.
 
	self wtsUnRegisterSessionNotification: handle.

]

{ #category : '*OSWindow-Morphic' }
SDLWindowsPlatform >> handleSystemCommand: aWindowMessage [

	aWindowMessage wParam = self restartUIManagerMenuID ifTrue: [
		UIManager default defer: [ OSWorldRenderer restart ] ].
	
	^ nil
]

{ #category : '*OSWindow-Morphic' }
SDLWindowsPlatform >> handleWTSessionChange: aWindowMessage [

	"WTS_SESSION_UNLOCK"
	aWindowMessage wParam = 8 ifTrue: [
		UIManager default defer: [ OSWorldRenderer restart ] ]. 

	^ nil
]
