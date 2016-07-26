test registration
testPrivateClassRegisterUnregister
	"Verify that when we register a class, the package organizer register it."
	| p a1 b1 |
	p := self createNewPackageNamed: 'P1'.
	a1 := self createNewClassNamed: #A1InPackageP1.
	p definedClassNames add: #A1InPackageP1.
		"ugly but necessary to test registerClass: independantly from addClassDefinition:"
		"No event should be raised."
	p registerClass: a1.
	self assert: (self packageClass organizer packageOf: a1) = p.
	p definedClassNames remove: #A1InPackageP1.
	p unregisterClass: a1.
	self assert: (self packageClass organizer packageOf: a1) isDefault.
	
	p definedClassNames add: #A1InPackageP1.
		"ugly but necessary to test registerClass: independant from addClassDefinition:"
	p registerClassName: a1 name.
	self assert: (self packageClass organizer packageOf: a1) = p.
	p definedClassNames remove: #A1InPackageP1.
	p unregisterClassName: a1 name.
	self assert: (self packageClass organizer packageOf: a1) isDefault
	