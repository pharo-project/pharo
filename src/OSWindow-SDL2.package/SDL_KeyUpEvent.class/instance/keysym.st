accessing structure variables
keysym
	"This method was automatically generated"
	^SDL_Keysym fromHandle: (handle referenceStructAt: OFFSET_KEYSYM length: 16)