examples
osWindow
	"
	self new osWindow
	"
	| attrs window |
	attrs := OSWindowAttributes new.
	attrs preferableDriver: OSSDL2Driver new;
		extent: Display extent.
	window := OSWindow createWithAttributes: attrs.
	window eventHandler: (OSWindowMorphicEventHandler for: World).
	window newFormRenderer: Display.
	^window

