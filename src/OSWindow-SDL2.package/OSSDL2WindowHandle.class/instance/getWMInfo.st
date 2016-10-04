private
getWMInfo
	| wmInfo |
	wmInfo := SDL_SysWMinfo new version: SDL_Version bindingVersion.
	handle getWMInfo: wmInfo.
	^ wmInfo