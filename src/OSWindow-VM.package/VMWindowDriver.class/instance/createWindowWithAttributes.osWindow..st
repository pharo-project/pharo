window creation
createWindowWithAttributes: attributes osWindow: osWindow
	^ OSVMWindowHandle new initWithAttributes: attributes; osWindow: osWindow; yourself