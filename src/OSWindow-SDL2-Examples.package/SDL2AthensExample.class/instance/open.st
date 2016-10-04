instance creation
open
	| attrs |
	attrs := OSWindowAttributes new.
	attrs 
		extent: self defaultExtent;
		resizable: false;
		position: 100@100.
		
	window := OSWindow createWithAttributes: attrs.
	
	renderer := window newAthensRenderer.

	self initializeFromSurface.

	window eventHandler: (self createEventHandler).
	
	drawingProcess := self createDrawingProcess