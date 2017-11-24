"
An OSWindowTest is a test class for testing the behavior of OSWindow
"
Class {
	#name : #OSWindowTest,
	#superclass : #TestCase,
	#category : #'OSWindow-Tests'
}

{ #category : #tests }
OSWindowTest >> testCreatingWindow [

	| window attributes |
	attributes := OSWindowAttributes new.
	attributes preferableDriver: OSNullWindowDriver new.
	window := OSWindow createWithAttributes: attributes.

	self assert: (window isValid).

]

{ #category : #tests }
OSWindowTest >> testNewWindowDefaults [

	| window |
	
	window := OSWindow newWithNullDriver.

	[
		self assert: (window isValid).
		self assert: window extent = OSWindowAttributes defaultExtent.
	] ensure: [ window destroy ]
		
	
]

{ #category : #tests }
OSWindowTest >> testSettingUpWindow [

	| window |
	
	window := OSWindow newWithNullDriver.
	
	[
		window extent: 150@150.
		self assert: (window extent = (150@150)).	
		self assert: (window isValid).
	] ensure: [ window destroy ].
	self assert: (window isValid not)
]
