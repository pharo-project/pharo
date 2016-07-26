tests
testCombinationOfSimpleShortcuts
	| shortcut otherShortcut combination |
	shortcut := $a command.
	otherShortcut := $b ctrl.
	
	combination := shortcut | otherShortcut.
	
	self assert: (combination includes: shortcut).
	self assert: (combination includes: otherShortcut).
	
	self assert: ((combination collect: #platform) allSatisfy: [ :p | p = #all ]).