pixel accessing
SDL_LockSurface: aSurface
	^ self ffiCall: #(int SDL_LockSurface( SDL_Surface* aSurface ))