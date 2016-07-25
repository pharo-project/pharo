window management
setHitTest: callback
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self 
		nbCall: #( int SDL_SetWindowHitTest( self , DraggableCallback callback, nil ) )