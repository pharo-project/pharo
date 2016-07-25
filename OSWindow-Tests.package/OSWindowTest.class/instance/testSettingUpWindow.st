tests
testSettingUpWindow

	| window |
	
	window := OSWindow newWithNullDriver.
	
	[
		window extent: 150@150.
		self assert: (window extent = (150@150)).	
		self assert: (window isValid).
	] ensure: [ window destroy ].
	self assert: (window isValid not)