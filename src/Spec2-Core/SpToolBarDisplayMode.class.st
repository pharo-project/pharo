"
Abstract superclass for different display modes for a toolbar
"
Class {
	#name : #SpToolBarDisplayMode,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Spec2-Core-Widgets'
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

{ #category : #configuring }
SpToolBarDisplayMode >> configureDropList: aSpDropListMorph item: aSpDropListPresenter [ 
	self flag: 'TODO: maybe customize the drop list to have a better look''n feel'.
]

{ #category : #configuring }
SpToolBarDisplayMode >> configureMorph: aMorph item: itemPresenter [
	aMorph configureWith: self item: itemPresenter
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
SpToolBarDisplayMode >> styleName [ 

	^ self subclassResponsibility
]

{ #category : #accessing }
SpToolBarDisplayMode >> width [

	^ self extent x
]
