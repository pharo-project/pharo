Class {
	#name : #GLMActionTest,
	#superclass : #TestCase,
	#category : 'Glamour-Tests-Core'
}

{ #category : #tests }
GLMActionTest >> testCondition [
	self 
		assert: (GLMAction new isAvailableOn: GLMPresentation new).
	self 
		deny: (GLMAction new 
			condition: [ false ];
			isAvailableOn: GLMPresentation new).
	self 
		deny: (GLMAction new 
			condition: [ :presentation :entity | false ];
			isAvailableOn: GLMPresentation new)
]

{ #category : #tests }
GLMActionTest >> testShortcutAsString [
	| action modifier |
	
	modifier := Smalltalk os menuShortcutModifierString.
	action := GLMAction new.
	action shortcut: $i.
	self assert: action shortcutAsString equals: modifier, '+i'.
	action shortcut: $I.
	self assert: action shortcutAsString equals: modifier, '+shift+i'.
	action shortcut: nil.
	self assert: action shortcutAsString isEmpty
]

{ #category : #tests }
GLMActionTest >> testShortcutFromKeymap [
	| action modifier |
	
	modifier := Smalltalk os defaultModifier.
	action := GLMAction new.
	action keymap: modifier + $i asShortcut.
	self assert: action keymap equals: modifier + $i asShortcut.
	action keymap: modifier + $i shift.
	self assert: action keymap equals: modifier + $i shift.
	action keymap: nil.
	self assert: action shortcutAsString isEmpty
]
