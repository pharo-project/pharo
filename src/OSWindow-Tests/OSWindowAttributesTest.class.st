"
An OSWindowAttributesTest is a test class for testing the behavior of OSWindowAttributes
"
Class {
	#name : #OSWindowAttributesTest,
	#superclass : #TestCase,
	#category : #'OSWindow-Tests-Tests'
}

{ #category : #tests }
OSWindowAttributesTest >> testDefaults [
	| attributes |
	attributes := OSWindowAttributes new.
	self assert: attributes position equals: attributes class defaultPosition.
	self assert: attributes extent equals: attributes class defaultExtent.
	self assert: attributes fullscreen equals: attributes class defaultFullscreen.
	self assert: attributes title equals: attributes class defaultTitle
]
