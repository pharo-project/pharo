converting
asPlatformSpecificHandle
	^ OSWindowPlatformSpecificHandle new
		platform: #win32;
		window: self window;
		surface: self hdc;
		yourself