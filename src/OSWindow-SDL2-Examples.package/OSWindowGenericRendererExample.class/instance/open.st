instance creation
open
	| attrs |
	attrs := OSWindowAttributes new.
	attrs
		extent: self defaultExtent;
		resizable: false;
		windowCentered: true.
		
	window := OSWindowAnimated createWithAttributes: attrs.
	
	renderer := window newGenericRenderer.
	self initializeWithRenderer.
	
	window eventHandler: (self createEventHandler).
