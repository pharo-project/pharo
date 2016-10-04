accessing structure variables
info
	"This method was automatically generated"
	^SDL_SysWMinfo_InfoUnion fromHandle: (handle referenceStructAt: 9 length: 16)