converting
asPlatformSpecificHandle
	^ OSWindowPlatformSpecificHandle new
		platform: #directfb;
		display: self dfb;
		window: self window;
		surface: self surface;
		yourself