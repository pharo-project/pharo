"
Abstract superclass for different display modes for a toolbar
"
Class {
	#name : #SpToolbarDisplayMode,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #accessing }
SpToolbarDisplayMode class >> default [
	^ self modeIconAndLabel
]

{ #category : #accessing }
SpToolbarDisplayMode class >> modeIcon [
	^ SpToolbarDisplayModeIcon uniqueInstance
]

{ #category : #accessing }
SpToolbarDisplayMode class >> modeIconAndLabel [
	^ SpToolbarDisplayModeIconAndLabel uniqueInstance
]

{ #category : #accessing }
SpToolbarDisplayMode class >> modeLabel [
	^ SpToolbarDisplayModeLabel uniqueInstance
]

{ #category : #'instance creation' }
SpToolbarDisplayMode class >> new [
	self error: 'Use #uniqueInstance'
]

{ #category : #'instance creation' }
SpToolbarDisplayMode class >> uniqueInstance [

	self = SpToolbarDisplayMode 
		ifTrue: [ self error: 'I''m abstract. Use one of my children' ].
	^ uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]
]

{ #category : #configuring }
SpToolbarDisplayMode >> configureButton: aButton item: aToolbarItem [
	self subclassResponsibility
]

{ #category : #configuring }
SpToolbarDisplayMode >> configureDropList: aSpDropListMorph item: aSpDropListPresenter [ 
	self flag: 'TODO: maybe customize the drop list to have a better look''n feel'.
]

{ #category : #configuring }
SpToolbarDisplayMode >> configureMorph: aMorph item: itemPresenter [
	aMorph configureWith: self item: itemPresenter
]

{ #category : #accessing }
SpToolbarDisplayMode >> extent [
	^ self subclassResponsibility
]

{ #category : #accessing }
SpToolbarDisplayMode >> height [

	^ self extent y
]

{ #category : #accessing }
SpToolbarDisplayMode >> styleName [ 

	^ self subclassResponsibility
]

{ #category : #accessing }
SpToolbarDisplayMode >> width [

	^ self extent x
]
