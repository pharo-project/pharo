testing
isVisible
	^ (self getFlags bitAnd: SDL_WINDOW_SHOWN) = SDL_WINDOW_SHOWN