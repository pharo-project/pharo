"
Specific SDL initialization for Windows
"
Class {
	#name : #SDLWindowsPlatform,
	#superclass : #SDLAbstractPlatform,
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #operations }
SDLWindowsPlatform >> afterSetWindowTitle: aString onWindow: aOSSDLWindow [
]

{ #category : #initialization }
SDLWindowsPlatform >> initPlatformSpecific [

	"For windows and Unix, we activate linearization.
	This does not work properly on OSX with retina display, blurrying the rendering"
	
	SDL2 setHint: SDL_HINT_RENDER_SCALE_QUALITY value: '1'
]

{ #category : #initialization }
SDLWindowsPlatform >> systemCursorConversionTable [

	^ {
		 #normal -> #SDL_SYSTEM_CURSOR_ARROW.
       #overEditableText -> #SDL_SYSTEM_CURSOR_IBEAM.
       #wait -> #SDL_SYSTEM_CURSOR_WAIT.
       #crossHair -> #SDL_SYSTEM_CURSOR_CROSSHAIR.
       "#SDL_SYSTEM_CURSOR_WAITARROW."
        #resizeTopLeft -> #SDL_SYSTEM_CURSOR_SIZENWSE.    
		  #resizeBottomRight -> #SDL_SYSTEM_CURSOR_SIZENWSE.
        #resizeBottomLeft -> #SDL_SYSTEM_CURSOR_SIZENESW.
        #resizeTopRight -> #SDL_SYSTEM_CURSOR_SIZENESW.
        #resizeLeft -> #SDL_SYSTEM_CURSOR_SIZEWE.
        #resizeRight -> #SDL_SYSTEM_CURSOR_SIZEWE.
        #resizeBottom -> #SDL_SYSTEM_CURSOR_SIZENS.
        #resizeTop -> #SDL_SYSTEM_CURSOR_SIZENS.
        #move -> #SDL_SYSTEM_CURSOR_SIZEALL.
       "SDL_SYSTEM_CURSOR_NO."
       #webLink -> #SDL_SYSTEM_CURSOR_HAND.	
		
		}
]
