surface
updateWindowSurfaceRects: rects num: numrects
	^ self ffiCall: #( int SDL_UpdateWindowSurfaceRects( self , void* rects , int numrects) )