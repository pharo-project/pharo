converting
asPlatformSpecificHandle
	^ OSWindowPlatformSpecificHandle new
		platform: #x11;
		display: self display;
		window: self window;
		yourself