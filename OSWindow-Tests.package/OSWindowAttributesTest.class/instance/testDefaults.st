tests
testDefaults
	| attributes |
	attributes := OSWindowAttributes new.
	self assert: attributes position = attributes class defaultPosition.
	self assert: attributes extent = attributes class defaultExtent.
	self assert: attributes fullscreen  = attributes class defaultFullscreen.
	self assert: attributes title = attributes class defaultTitle.