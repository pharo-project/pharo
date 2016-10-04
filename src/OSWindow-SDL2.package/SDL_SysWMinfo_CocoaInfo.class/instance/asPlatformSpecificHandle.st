converting
asPlatformSpecificHandle
	^ OSWindowPlatformSpecificHandle new
		platform: #cocoa;
		window: self window;
		yourself