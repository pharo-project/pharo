accessing structure variables
dfb
	"This method was automatically generated"
	^SDL_SysWMinfo_DirectFBInfo fromHandle: (handle referenceStructAt: 1 length: 12)