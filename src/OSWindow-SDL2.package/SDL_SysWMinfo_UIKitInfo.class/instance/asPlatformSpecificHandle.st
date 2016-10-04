converting
asPlatformSpecificHandle
	^ OSWindowPlatformSpecificHandle new
		platform: #uikit;
		window: self window;
		surface: self framebuffer;
		yourself