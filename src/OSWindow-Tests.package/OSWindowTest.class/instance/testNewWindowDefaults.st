tests
testNewWindowDefaults

	| window |
	
	window := OSWindow newWithNullDriver.

	[
		self assert: (window isValid).
		self assert: window extent = OSWindowAttributes defaultExtent.
	] ensure: [ window destroy ]
		
	