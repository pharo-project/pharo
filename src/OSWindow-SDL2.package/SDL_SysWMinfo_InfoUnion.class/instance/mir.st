accessing structure variables
mir
	"This method was automatically generated"
	^SDL_SysWMinfo_MirInfo fromHandle: (handle referenceStructAt: 1 length: 16)