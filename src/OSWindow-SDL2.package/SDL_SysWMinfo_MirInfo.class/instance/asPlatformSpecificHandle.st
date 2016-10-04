converting
asPlatformSpecificHandle
	^ OSWindowPlatformSpecificHandle new
		platform: #mir;
		display: self connection;
		surface: self surface;
		yourself