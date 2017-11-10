"
This is an abstract class for test cases for the Keymapping project. 

It includes some utility methods to make it simpler to test different key events.

When tests are run this class will ensure that the current Keymap global repository is swiched out with a dummy one to not interfer with the running system.  (see #setUp/#tearDown).



"
Class {
	#name : #AbstractKeymappingTest,
	#superclass : #TestCase,
	#instVars : [
		'default'
	],
	#category : #Keymapping-Tests
}

{ #category : #utilities }
AbstractKeymappingTest >> eventKey: character [ 
	^ self eventKey: character
		alt: false
		ctrl: false
		command: false
		shift: false
]

{ #category : #utilities }
AbstractKeymappingTest >> eventKey: character alt: aBoolean [
	^ self eventKey: character
		alt: aBoolean
		ctrl: false
		command: false
		shift: false
]

{ #category : #utilities }
AbstractKeymappingTest >> eventKey: character alt: useAlt ctrl: useCtrl command: useCommand shift: useShift [
	| event modifier code |
	event := KeyboardEvent new.
	modifier := 0.
	
	useShift 
		ifTrue: [ modifier := modifier + 8].
	useCtrl 
		ifTrue: [ modifier := modifier + 16].
	useAlt 
		ifTrue: [ modifier := modifier + 32].
	useCommand
		ifTrue: [ modifier := modifier + 64].
	
	code := character asInteger.
	event 
		setType: #keystroke
		buttons: modifier
		position:  0@0
		keyValue: code
		charCode: code
		hand: nil 
		stamp: Time now.
	^ event 	
]

{ #category : #utilities }
AbstractKeymappingTest >> eventKey: character command: aBoolean [
	^ self eventKey: character
		alt: false
		ctrl: false
		command: aBoolean
		shift: false
]

{ #category : #utilities }
AbstractKeymappingTest >> eventKey: character ctrl: aBoolean [
	^ self eventKey: character
		alt: false
		ctrl: aBoolean
		command: false
		shift: false
]

{ #category : #utilities }
AbstractKeymappingTest >> eventKey: character shift: aBoolean [
	^ self eventKey: character
		alt: false
		ctrl: false
		command: false
		shift: aBoolean
]

{ #category : #running }
AbstractKeymappingTest >> setUp [
	default := KMRepository default.
	KMRepository default: KMRepository new
]

{ #category : #running }
AbstractKeymappingTest >> tearDown [
	KMRepository default: default
]
