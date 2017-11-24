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
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_DestroyWindow ( self ) )
]

{ #category : #accessing }
SDL_Window >> fullscreen: flags [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_SetWindowFullscreen ( self , Uint32 flags ) )
]

{ #category : #surface }
SDL_Window >> getFlags [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( Uint32 SDL_GetWindowFlags ( self ) )
]

{ #category : #accessing }
SDL_Window >> getPositionX: x y: y [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_GetWindowPosition ( self , int* x , int* y ) )
]

{ #category : #accessing }
SDL_Window >> getSizeW: w h: h [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_GetWindowSize ( self , int* w , int* h ) )
]

{ #category : #'window management' }
SDL_Window >> getWMInfo: infos [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( bool SDL_GetWindowWMInfo( self, SDL_SysWMinfo* infos) )
]

{ #category : #surface }
SDL_Window >> getWindowSurface [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( SDL_Surface* SDL_GetWindowSurface( self ) )
]

{ #category : #'window management' }
SDL_Window >> hide [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_HideWindow( self ) )
]

{ #category : #accessing }
SDL_Window >> icon: surface [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_SetWindowIcon ( self , SDL_Surface* surface ) )
]

{ #category : #'window management' }
SDL_Window >> maximize [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_MaximizeWindow( self ) )
]

{ #category : #'window management' }
SDL_Window >> minimize [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_MinimizeWindow( self ) )
]

{ #category : #private }
SDL_Window >> primCreateRenderer: driverIndex flags: flags [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( SDL_Renderer SDL_CreateRenderer ( self , int driverIndex , Uint32 flags ) )
]

{ #category : #'window management' }
SDL_Window >> restore [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_RestoreWindow( self ) )
]

{ #category : #'window management' }
SDL_Window >> setHitTest: callback [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self 
		nbCall: #( int SDL_SetWindowHitTest( self , DraggableCallback callback, nil ) )
]

{ #category : #accessing }
SDL_Window >> setPositionX: x y: y [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_SetWindowPosition ( self , int x , int y ) )
]

{ #category : #accessing }
SDL_Window >> setSizeW: w h: h [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_SetWindowSize ( self , int w , int h ) )
]

{ #category : #'window management' }
SDL_Window >> show [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_ShowWindow( self ) )
]

{ #category : #accessing }
SDL_Window >> title [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( String SDL_GetWindowTitle ( self ) )
]

{ #category : #accessing }
SDL_Window >> title: title [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_SetWindowTitle ( self , String title ) )
]

{ #category : #'window management' }
SDL_Window >> toggleBorder: bordered [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( void SDL_SetWindowBordered( self, bool bordered ) )
]

{ #category : #surface }
SDL_Window >> updateWindowSurface [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_UpdateWindowSurface( self ) )
]

{ #category : #surface }
SDL_Window >> updateWindowSurfaceRects: rects num: numrects [
	^ self ffiCall: #( int SDL_UpdateWindowSurfaceRects( self , void* rects , int numrects) )
]

{ #category : #accessing }
SDL_Window >> windowID [
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( Uint32 SDL_GetWindowID ( self ) )
]
