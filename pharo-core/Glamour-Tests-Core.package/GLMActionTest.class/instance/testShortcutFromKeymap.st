tests
testShortcutFromKeymap
	| action modifier |
	
	modifier := Smalltalk os defaultModifier.
	action := GLMAction new.
	action keymap: modifier + $i asShortcut.
	self assert: action keymap equals: modifier + $i asShortcut.
	action keymap: modifier + $i shift.
	self assert: action keymap equals: modifier + $i shift.
	action keymap: nil.
	self assert: action shortcutAsString isEmpty