converting
asPlatformSpecificHandle
	^ OSWindowPlatformSpecificHandle new
		platform: #winrt;
		window: self window;
		yourself