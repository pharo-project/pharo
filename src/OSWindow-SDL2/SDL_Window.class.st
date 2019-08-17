"
I represent a handle of SDL2 window, coming from SDL2 library
"
Class {
	#name : #'SDL_Window',
	#superclass : #SDL2Handle,
	#pools : [
		'SDL2Constants',
		'SDL2Types'
	],
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #finalization }
SDL_Window class >> destroyWindow: handle [
	self ffiCall: #( void SDL_DestroyWindow ( void *handle ) ) 
]

{ #category : #finalization }
SDL_Window class >> finalizeResourceData: aTuple [
	| handle windowId |
	
	handle := aTuple first.
	handle isNull ifTrue: [ ^ self ].

	windowId := aTuple second.
	OSSDL2Driver current unregisterWindowWithId: windowId.	
	self destroyWindow: handle.
	handle beNull
]

{ #category : #rendering }
SDL_Window >> createDefaultRenderer [
	^ self createRenderer: -1 flags: 0
]

{ #category : #rendering }
SDL_Window >> createRenderer: driverIndex flags: flags [

	^ (self primCreateRenderer: driverIndex flags: flags) initialize.
]

{ #category : #initialize }
SDL_Window >> destroy [
	OSSDL2Driver current unregisterWindowWithId: self windowID.
	self primDestroy.
	handle beNull
]

{ #category : #accessing }
SDL_Window >> fullscreen: flags [
	^ self ffiCall: #( int SDL_SetWindowFullscreen ( self , Uint32 flags ) )
]

{ #category : #surface }
SDL_Window >> getFlags [
	^ self ffiCall: #( Uint32 SDL_GetWindowFlags ( self ) )
]

{ #category : #accessing }
SDL_Window >> getPositionX: x y: y [
	^ self ffiCall: #( void SDL_GetWindowPosition ( self , int* x , int* y ) )
]

{ #category : #accessing }
SDL_Window >> getSizeW: w h: h [
	^ self ffiCall: #( void SDL_GetWindowSize ( self , int* w , int* h ) )
]

{ #category : #'window management' }
SDL_Window >> getWMInfo: infos [
	^ self ffiCall: #( bool SDL_GetWindowWMInfo( self, SDL_SysWMinfo* infos) )
]

{ #category : #surface }
SDL_Window >> getWindowSurface [
	^ self ffiCall: #( SDL_Surface* SDL_GetWindowSurface( self ) )
]

{ #category : #'window management' }
SDL_Window >> hide [
	^ self ffiCall: #( void SDL_HideWindow( self ) )
]

{ #category : #accessing }
SDL_Window >> icon: surface [
	^ self ffiCall: #( void SDL_SetWindowIcon ( self , SDL_Surface* surface ) )
]

{ #category : #'text input' }
SDL_Window >> isTextInputActive [
	^ self ffiCall: #(bool SDL_IsTextInputActive(void))
]

{ #category : #'window management' }
SDL_Window >> maximize [
	^ self ffiCall: #( void SDL_MaximizeWindow( self ) )
]

{ #category : #'window management' }
SDL_Window >> minimize [
	^ self ffiCall: #( void SDL_MinimizeWindow( self ) )
]

{ #category : #private }
SDL_Window >> primCreateRenderer: driverIndex flags: flags [
	^ self ffiCall: #( SDL_Renderer SDL_CreateRenderer ( self , int driverIndex , Uint32 flags ) )
]

{ #category : #private }
SDL_Window >> primDestroy [
	^ self ffiCall: #( void SDL_DestroyWindow ( self ) )
]

{ #category : #'window management' }
SDL_Window >> raise [

	^ self ffiCall: #( void SDL_RaiseWindow( self ) )
]

{ #category : #'external resource management' }
SDL_Window >> resourceData [
	^ {self getHandle. self windowID }
]

{ #category : #'window management' }
SDL_Window >> restore [
	^ self ffiCall: #( void SDL_RestoreWindow( self ) )
]

{ #category : #'window management' }
SDL_Window >> setHitTest: callback [
	^ self ffiCall: #( int SDL_SetWindowHitTest( self , DraggableCallback callback, nil ) )
]

{ #category : #accessing }
SDL_Window >> setPositionX: x y: y [
	^ self ffiCall: #( void SDL_SetWindowPosition ( self , int x , int y ) )
]

{ #category : #accessing }
SDL_Window >> setSizeW: w h: h [
	^ self ffiCall: #( void SDL_SetWindowSize ( self , int w , int h ) )
]

{ #category : #'window management' }
SDL_Window >> show [
	^ self ffiCall: #( void SDL_ShowWindow( self ) )
]

{ #category : #'text input' }
SDL_Window >> startTextInput [
	^ self ffiCall: #(void SDL_StartTextInput(void))
]

{ #category : #'text input' }
SDL_Window >> stopTextInput [
	^ self ffiCall: #(void SDL_StopTextInput(void))
]

{ #category : #accessing }
SDL_Window >> title [
	^ self ffiCall: #( String SDL_GetWindowTitle ( self ) )
]

{ #category : #accessing }
SDL_Window >> title: title [
	^ self ffiCall: #( void SDL_SetWindowTitle ( self , String title ) )
]

{ #category : #'window management' }
SDL_Window >> toggleBorder: bordered [
	^ self ffiCall: #( void SDL_SetWindowBordered( self, bool bordered ) )
]

{ #category : #surface }
SDL_Window >> updateWindowSurface [
	^ self ffiCall: #( int SDL_UpdateWindowSurface( self ) )
]

{ #category : #surface }
SDL_Window >> updateWindowSurfaceRects: rects num: numrects [
	^ self ffiCall: #( int SDL_UpdateWindowSurfaceRects( self , void* rects , int numrects) )
]

{ #category : #accessing }
SDL_Window >> windowID [
	^ self ffiCall: #( Uint32 SDL_GetWindowID ( self ) )
]
