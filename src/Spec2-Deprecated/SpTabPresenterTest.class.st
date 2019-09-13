Class {
	#name : #SpTabPresenterTest,
	#superclass : #SpSmokeTest,
	#instVars : [
		'manager'
	],
	#category : #'Spec2-Deprecated-Tests'
}

{ #category : #deprecation }
SpTabPresenterTest class >> isDeprecated [
	^ true
]

{ #category : #running }
SpTabPresenterTest >> classToTest [
	^ SpTabPresenter
]

{ #category : #running }
SpTabPresenterTest >> initializeTestedInstance [
	manager := SpTabManagerPresenter new
]

{ #category : #'instance creation' }
SpTabPresenterTest >> redMorphTab [
	presenter
		label: 'Red Morph';
		icon: (presenter iconNamed: #smallBackIcon);
		contents:
			(Morph new
				color: Color red;
				yourself).
]

{ #category : #tests }
SpTabPresenterTest >> testChangeLabel [
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
SpTabPresenterTest >> testSetOwner [
	"issue 16553"

	| button |
	button := SpButtonPresenter new.
	self assert: button owner isNil.
	
	presenter presenter: button.
	self assert: button owner equals: presenter
]
