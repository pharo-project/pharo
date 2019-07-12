"
Abstract superclass for different display modes for a toolbar
"
Class {
	#name : #SpToolBarDisplayMode,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Spec2-PolyWidgets-ToolBar'
}

{ #category : #accessing }
SpToolBarDisplayMode class >> default [
	^ self modeIconAndLabel
]

{ #category : #accessing }
SpToolBarDisplayMode class >> modeIcon [
	^ SpToolBarDisplayModeIcon uniqueInstance
]

{ #category : #accessing }
SpToolBarDisplayMode class >> modeIconAndLabel [
	^ SpToolBarDisplayModeIconAndLabel uniqueInstance
]

{ #category : #accessing }
SpToolBarDisplayMode class >> modeLabel [
	^ SpToolBarDisplayModeLabel uniqueInstance
]

{ #category : #'instance creation' }
SpToolBarDisplayMode class >> new [
	self error: 'Use #uniqueInstance'
]

{ #category : #'instance creation' }
SpToolBarDisplayMode class >> uniqueInstance [

	self = SpToolBarDisplayMode 
		ifTrue: [ self error: 'I''m abstract. Use one of my children' ].
	^ uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]
]

{ #category : #configuring }
SpToolBarDisplayMode >> configureButton: aButton item: aToolbarItem [
	self subclassResponsibility
]

{ #category : #accessing }
SpToolBarDisplayMode >> extent [
	^ self subclassResponsibility
]

{ #category : #accessing }
SpToolBarDisplayMode >> height [

	^ self extent y
]

{ #category : #accessing }
SpToolBarDisplayMode >> width [

	^ self extent x
]
