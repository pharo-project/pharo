accessing structure variables
format
	"This method was automatically generated"
	^SDL_PixelFormat fromHandle: (handle pointerAt: OFFSET_FORMAT)