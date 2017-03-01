accessing structure variables
version
	"This method was automatically generated"
	^SDL_Version fromHandle: (handle referenceStructAt: OFFSET_VERSION length: 3)