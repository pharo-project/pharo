"
An OSWindowAttributesTest is a test class for testing the behavior of OSWindowAttributes
"
Class {
	#name : #OSWindowAttributesTest,
	#superclass : #TestCase,
	#category : #OSWindow-Tests
}

{ #category : #tests }
OSWindowAttributesTest >> testDefaults [
	| attributes |
	attributes := OSWindowAttributes new.
	self assert: attributes position = attributes class defaultPosition.
	self assert: attributes extent = attributes class defaultExtent.
	self assert: attributes fullscreen  = attributes class defaultFullscreen.
	self assert: attributes title = attributes class defaultTitle.
]
