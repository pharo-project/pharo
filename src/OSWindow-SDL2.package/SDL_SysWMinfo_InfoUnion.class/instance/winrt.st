accessing structure variables
winrt
	"This method was automatically generated"
	^SDL_SysWMinfo_WindowsRTInfo fromHandle: (handle referenceStructAt: 1 length: 8)