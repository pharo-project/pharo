"
Specific SDL initialization for Windows
"
Class {
	#name : 'SDLWindowsPlatform',
	#superclass : 'SDLAbstractPlatform',
	#category : 'OSWindow-SDL2-Platforms',
	#package : 'OSWindow-SDL2',
	#tag : 'Platforms'
}

{ #category : 'operations' }
SDLWindowsPlatform class >> enableDebugWindowMenu: hwnd [

	self ffiCall: #(int enableDebugWindowMenu(void* hwnd)) module: 'PharoVMCore.dll'
]

{ #category : 'operations' }
SDLWindowsPlatform >> addRestartUIManagerOption: handle [

    | windowMenu | 

	windowMenu := self getSystemMenu: handle.
	self 
		appendMenu: windowMenu 
		flags: 0 "MF_STRING" 
		idNewItem: self restartUIManagerMenuID 
		newItemString: 'Restart World Window' asWin32WideString

]

{ #category : 'operations' }
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

{ #category : 'operations' }
SDLWindowsPlatform >> afterSetWindowTitle: aString onWindow: aOSSDLWindow [
]

{ #category : 'operations' }
SDLWindowsPlatform >> appendMenu: hMenu flags: uFlags idNewItem: uIDNewItem newItemString: lpNewItem [

	self 
			ffiCall: #(bool AppendMenuW(void* hMenu, uint uFlags, uint uIDNewItem, Win32WideString lpNewItem))
			module: 'User32.dll'

]

{ #category : 'operations' }
SDLWindowsPlatform >> beforeMainPharoWindowClosed: aOSSDLWindow [

	| sdlWindowInfo handle |
	sdlWindowInfo := aOSSDLWindow backendWindow getWMInfo.
	handle := sdlWindowInfo info win window.
 
	self wtsUnRegisterSessionNotification: handle.

]

{ #category : 'initialization' }
SDLWindowsPlatform >> enableDebugWindowMenu: handle [

	^ [ self class enableDebugWindowMenu: handle ] 
		onErrorDo: [ 
			"There was an error installing the support for debug Window, the VM is not new enough."
		   ]
]

{ #category : 'operations' }
SDLWindowsPlatform >> getActiveConsoleSessionId [

	self ffiCall: #(int32 WTSGetActiveConsoleSessionId()) module: 'Kernel32.dll'
]

{ #category : 'operations' }
SDLWindowsPlatform >> getRemoteSessionId [

	^ (self getSystemMetrics: "SM_REMOTESESSION" 16r1000) = 0 
		ifTrue: [ nil ]
		ifFalse: [ self getActiveConsoleSessionId ]
]

{ #category : 'operations' }
SDLWindowsPlatform >> getSystemMenu: handle [

	self ffiCall: #(void* GetSystemMenu(void* handle, bool false)) module: 'User32.dll'
]

{ #category : 'operations' }
SDLWindowsPlatform >> getSystemMetrics: nIndex [

	self
		ffiCall: #( int GetSystemMetrics #( int nIndex ) )
		module: 'User32.dll'
]

{ #category : 'event handling' }
SDLWindowsPlatform >> handleSystemCommand: aWindowMessage [

	aWindowMessage wParam = self restartUIManagerMenuID ifTrue: [
		UIManager default defer: [ OSWorldRenderer restart ] ].
	
	^ nil
]

{ #category : 'event handling' }
SDLWindowsPlatform >> handleWTSessionChange: aWindowMessage [

	"WTS_SESSION_UNLOCK"
	aWindowMessage wParam = 8 ifTrue: [
		UIManager default defer: [ OSWorldRenderer restart ] ]. 

	^ nil
]

{ #category : 'initialization' }
SDLWindowsPlatform >> initPlatformSpecific [

	"For windows and Unix, we activate linearization.
	This does not work properly on OSX with retina display, blurrying the rendering"

	SDL2 setHint: SDL_HINT_RENDER_SCALE_QUALITY value: '1'
]

{ #category : 'operations' }
SDLWindowsPlatform >> restartUIManagerMenuID [

	^ 16rE002 "Next Value for menu item"
]

{ #category : 'initialization' }
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

{ #category : 'operations' }
SDLWindowsPlatform >> wtsRegisterSessionNotification: handle [

	"Notify for this session NOTIFY_FOR_THIS_SESSION = 0"
	self ffiCall: #(bool WTSRegisterSessionNotification(void* handle, int32 0)) module: 'Wtsapi32.dll'
]

{ #category : 'operations' }
SDLWindowsPlatform >> wtsUnRegisterSessionNotification: handle [
	
	self ffiCall: #(bool WTSUnRegisterSessionNotification(void* handle)) module: 'Wtsapi32.dll'
]
