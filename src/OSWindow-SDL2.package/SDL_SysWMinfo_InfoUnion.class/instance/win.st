accessing structure variables
win
	"This method was automatically generated"
	^SDL_SysWMinfo_WindowsInfo fromHandle: (handle referenceStructAt: 1 length: 8)