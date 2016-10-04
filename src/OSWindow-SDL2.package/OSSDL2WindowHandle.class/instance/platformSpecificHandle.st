accessing
platformSpecificHandle
	| wmInfo platformID |
	wmInfo := self getWMInfo.
	platformID := wmInfo subsystem.
	platformID = SDL_SYSWM_WINDOWS ifTrue: [ ^ wmInfo info win asPlatformSpecificHandle ].
	platformID = SDL_SYSWM_X11 ifTrue: [ ^ wmInfo info x11 asPlatformSpecificHandle ].
	platformID = SDL_SYSWM_DIRECTFB ifTrue: [ ^ wmInfo info dfb asPlatformSpecificHandle ].
	platformID = SDL_SYSWM_COCOA ifTrue: [ ^ wmInfo info cocoa asPlatformSpecificHandle ].
	platformID = SDL_SYSWM_UIKIT ifTrue: [ ^ wmInfo info uikit asPlatformSpecificHandle ].
	platformID = SDL_SYSWM_WAYLAND ifTrue: [ ^ wmInfo info wl asPlatformSpecificHandle ].
	platformID = SDL_SYSWM_MIR ifTrue: [ ^ wmInfo info mir asPlatformSpecificHandle ].
	platformID = SDL_SYSWM_WINRT ifTrue: [ ^ wmInfo info winrt asPlatformSpecificHandle ].
	platformID = SDL_SYSWM_ANDROID ifTrue: [ ^ wmInfo info android asPlatformSpecificHandle ].
	
	self error: 'Trying to get a platform specific handle for an unsupported SDL2 platform.'