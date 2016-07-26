tests
testShortcutIsSimpleCombination
	| shortcut |
	shortcut := $a command.
	self assert: (shortcut includes: shortcut).
	self assert: shortcut shortcut equals: shortcut.