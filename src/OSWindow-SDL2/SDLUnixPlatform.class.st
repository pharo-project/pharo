"
I execute specific operations on SDL for Linux 64 (Unix term is used to keep OSPlatform convention)

"
Class {
	#name : 'SDLUnixPlatform',
	#superclass : 'SDLAbstractPlatform',
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'operations' }
SDLUnixPlatform >> afterMainPharoWindowCreated: aOSSDLWindow [
]

{ #category : 'operations' }
SDLUnixPlatform >> afterSetWindowTitle: aString onWindow: aOSSDLWindow [
]

{ #category : 'initialization' }
SDLUnixPlatform >> initPlatformSpecific [

	SDL2 setHint: SDL_HINT_RENDER_SCALE_QUALITY value: '1'.

	"On Linux, set SDL to not disable WM compositing"
	SDL2 setHint: SDL_HINT_VIDEO_X11_NET_WM_BYPASS_COMPOSITOR value: '0'
]
