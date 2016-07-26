detector registering
registerGesture: aGesture
	registeredGestures add: aGesture.
	aGesture eventHandler: self