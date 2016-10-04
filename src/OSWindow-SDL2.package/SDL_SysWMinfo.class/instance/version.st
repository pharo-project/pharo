accessing structure variables
version
	"This method was automatically generated"
	^SDL_Version fromHandle: (handle referenceStructAt: 1 length: 3)