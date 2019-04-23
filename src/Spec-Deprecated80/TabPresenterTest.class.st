Class {
	#name : #TabPresenterTest,
	#superclass : #SpecSmokeTestCase,
	#instVars : [
		'manager'
	],
	#category : #'Spec-Deprecated80-Tests'
}

{ #category : #deprecation }
TabPresenterTest class >> isDeprecated [
	^ true
]

{ #category : #running }
TabPresenterTest >> classToTest [
	^ TabPresenter
]

{ #category : #running }
TabPresenterTest >> initializeTestedInstance [
	manager := TabManagerPresenter new
]

{ #category : #'instance creation' }
TabPresenterTest >> redMorphTab [
	presenter
		label: 'Red Morph';
		icon: (presenter iconNamed: #smallBackIcon);
		contents:
			(Morph new
				color: Color red;
				yourself).
]

{ #category : #tests }
TabPresenterTest >> testChangeLabel [
	"issue 14885"

	self redMorphTab.
	manager addTab: presenter.
	self assert: presenter label equals: 'Red Morph'.
	presenter label: 'Change Before Opening'.
	self assert: presenter label equals: 'Change Before Opening'.
	window := manager openWithSpec.
	presenter label: 'Change After Opening'.
	self assert: presenter label equals: 'Change After Opening'
]

{ #category : #tests }
TabPresenterTest >> testSetOwner [
	"issue 16553"

	| button |
	button := ButtonPresenter new.
	self assert: button owner isNil.
	
	presenter presenter: button.
	self assert: button owner equals: presenter
]
