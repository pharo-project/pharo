session management
recreateOSWindow
	| attributes driver |
	session := Smalltalk session.
	attributes := OSWindowAttributes new.
	attributes
		extent: self extent;
		title: Smalltalk shortImageName;
		icon: (self iconNamed: #pharoIcon);
		resizable: isResizeable.
	self updateDisplay.
	driver := self pickMostSuitableWindowDriver.
	attributes preferableDriver: driver.
	osWindow := OSWindow createWithAttributes: attributes.
	osWindow newFormRenderer: display.
	osWindow eventHandler: (OSWindowMorphicEventHandler for: self)