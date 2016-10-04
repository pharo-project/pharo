accessing structure variables
wl
	"This method was automatically generated"
	^SDL_SysWMinfo_WaylandInfo fromHandle: (handle referenceStructAt: 1 length: 12)