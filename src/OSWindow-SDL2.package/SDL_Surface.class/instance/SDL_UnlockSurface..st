pixel accessing
SDL_UnlockSurface: aSurface
	^ self ffiCall: #( int SDL_UnlockSurface( SDL_Surface* aSurface ) )