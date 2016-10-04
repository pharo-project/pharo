field definition
fieldsDesc
	"
	self rebuildFieldAccessors
	"
	^#(
		SDL_SysWMinfo_WindowsInfo win;
		SDL_SysWMinfo_WindowsRTInfo winrt;
		SDL_SysWMinfo_X11Info x11;
		SDL_SysWMinfo_DirectFBInfo dfb;
		SDL_SysWMinfo_CocoaInfo cocoa;
		SDL_SysWMinfo_UIKitInfo uikit;
		SDL_SysWMinfo_WaylandInfo wl;
		SDL_SysWMinfo_MirInfo mir;
		SDL_SysWMinfo_AndroidInfo android;
 	)