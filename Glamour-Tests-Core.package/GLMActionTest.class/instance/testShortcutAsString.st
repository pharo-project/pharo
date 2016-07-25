tests
testShortcutAsString
	| action modifier |
	
	modifier := Smalltalk os menuShortcutModifierString.
	action := GLMAction new.
	action shortcut: $i.
	self assert: action shortcutAsString equals: modifier, '+i'.
	action shortcut: $I.
	self assert: action shortcutAsString equals: modifier, '+shift+i'.
	action shortcut: nil.
	self assert: action shortcutAsString isEmpty