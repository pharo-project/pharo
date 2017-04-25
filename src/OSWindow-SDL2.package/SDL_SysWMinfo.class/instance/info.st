accessing structure variables
info
	"This method was automatically generated"
	^SDL_SysWMinfo_InfoUnion fromHandle: (handle referenceStructAt: OFFSET_INFO length: 24)