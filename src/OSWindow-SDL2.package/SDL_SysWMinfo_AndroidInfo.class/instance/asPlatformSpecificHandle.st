converting
asPlatformSpecificHandle
	^ OSWindowPlatformSpecificHandle new
		platform: #android;
		window: self window;
		surface: self surface;
		yourself