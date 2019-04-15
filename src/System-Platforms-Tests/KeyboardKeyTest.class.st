"
A KeyboardKeyTest is a test class for testing the behavior of KeyboardKey
"
Class {
	#name : #KeyboardKeyTest,
	#superclass : #TestCase,
	#category : #'System-Platforms-Tests-Utilities'
}

{ #category : #tests }
KeyboardKeyTest >> testNamed [
	"These assertions have been written using data available in KeyboardKey class>>#initializeKeyTable"
	self assert: (KeyboardKey named: 'Shift_L') equals: (KeyboardKey value: 65505).
	
	self assert: (KeyboardKey named: 'Left') equals: (KeyboardKey value: 65361).
	
	self assert: (KeyboardKey named: 'Alt_R') equals: (KeyboardKey value: 65514).
	
	self assert: (KeyboardKey named: 'a') equals: (KeyboardKey value: 97).
	
	self assert: (KeyboardKey named: 'A') equals: (KeyboardKey value: 65).
]

{ #category : #tests }
KeyboardKeyTest >> testValue [
	| key |
	key := KeyboardKey value: 65505.
	self assert: key value equals: 65505.
	self assert: key name equals: 'Shift_L'.
	
	key := KeyboardKey value: 65361.
	self assert: key value equals: 65361.
	self assert: key name equals: 'Left'.
	
	key := KeyboardKey value: 65514.
	self assert: key value equals: 65514.
	self assert: key name equals: 'Alt_R'.
	
	key := KeyboardKey value: 97.
	self assert: key value equals: 97.
	self assert: key name equals: 'a'.
	
	key := KeyboardKey value: 65.
	self assert: key value equals: 65.
	self assert: key name equals: 'A'.
]
