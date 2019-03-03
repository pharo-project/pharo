"
Abstract superclass for different display modes for a toolbar
"
Class {
	#name : #ToolBarDisplayMode,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Spec-PolyWidgets-ToolBar'
}

{ #category : #accessing }
ToolBarDisplayMode class >> default [
	^ self modeIconAndLabel
]

{ #category : #accessing }
ToolBarDisplayMode class >> modeIcon [
	^ ToolBarDisplayModeIcon uniqueInstance
]

{ #category : #accessing }
ToolBarDisplayMode class >> modeIconAndLabel [
	^ ToolBarDisplayModeIconAndLabel uniqueInstance
]

{ #category : #accessing }
ToolBarDisplayMode class >> modeLabel [
	^ ToolBarDisplayModeLabel uniqueInstance
]

{ #category : #'instance creation' }
ToolBarDisplayMode class >> new [
	self error: 'Use #uniqueInstance'
]

{ #category : #'instance creation' }
ToolBarDisplayMode class >> uniqueInstance [

	self = ToolBarDisplayMode 
		ifTrue: [ self error: 'I''m abstract. Use one of my children' ].
	^ uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]
]

{ #category : #configuring }
ToolBarDisplayMode >> configureButton: aButton item: aToolbarItem [
	self subclassResponsibility
]

{ #category : #accessing }
ToolBarDisplayMode >> extent [
	^ self subclassResponsibility
]

{ #category : #accessing }
ToolBarDisplayMode >> height [

	^ self extent y
]

{ #category : #accessing }
ToolBarDisplayMode >> width [

	^ self extent x
]
