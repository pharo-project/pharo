converting
asPlatformSpecificHandle
	^ OSWindowPlatformSpecificHandle new
		platform: #wayland;
		display: self display;
		window: self shell_surface;
		surface: self surface;
		yourself