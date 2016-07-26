tests
testPlatformDependentShortcut
	| shortcut |
	shortcut := $a ctrl win.
	
	self assert: (shortcut shortcut = $a ctrl).
	
	self assert: shortcut platform equals: #Windows.