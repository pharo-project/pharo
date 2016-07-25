instance creation
newFromPixels: pixels width: width height: height pitch: pitch
	^ (self
		primImageFromData: pixels
		width: width
		height: height
		pitch: pitch) initialize