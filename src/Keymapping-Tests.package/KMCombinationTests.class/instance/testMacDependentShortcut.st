tests
testMacDependentShortcut
	| shortcut |
	shortcut := $a ctrl mac.
	
	self assert: (shortcut shortcut = $a ctrl).
	
	self assert: shortcut platform equals: #MacOSX.