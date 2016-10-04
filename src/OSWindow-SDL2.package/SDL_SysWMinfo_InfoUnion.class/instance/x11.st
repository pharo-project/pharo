accessing structure variables
x11
	"This method was automatically generated"
	^SDL_SysWMinfo_X11Info fromHandle: (handle referenceStructAt: 1 length: 8)