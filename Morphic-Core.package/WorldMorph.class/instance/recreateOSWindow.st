session management
recreateOSWindow
	| attributes driver |
	session := Smalltalk session.
	attributes := OSWindowAttributes new.
	attributes
		extent: self extent;
		title: Smalltalk shortImageName;
		icon: (Smalltalk ui icons iconNamed: #pharoIcon).
	driver := self pickMostSuitableWindowDriver.
	attributes preferableDriver: driver.
	osWindow := OSWindow createWithAttributes: attributes.
	osWindow newFormRenderer: Display.
	osWindow eventHandler: (OSWindowMorphicEventHandler for: self)