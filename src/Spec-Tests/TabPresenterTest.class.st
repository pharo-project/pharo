Class {
	#name : #TabPresenterTest,
	#superclass : #SpecTestCase,
	#instVars : [
		'manager'
	],
	#category : #'Spec-Tests-Core-Widgets'
}

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
	testedInstance
		label: 'Red Morph';
		icon: (testedInstance iconNamed: #smallBackIcon);
		contents:
			(Morph new
				color: Color red;
				yourself).
]

{ #category : #tests }
TabPresenterTest >> testChangeLabel [
	"issue 14885"

	self redMorphTab.
	manager addTab: testedInstance.
	self assert: testedInstance label equals: 'Red Morph'.
	testedInstance label: 'Change Before Opening'.
	self assert: testedInstance label equals: 'Change Before Opening'.
	window := manager openWithSpec.
	testedInstance label: 'Change After Opening'.
	self assert: testedInstance label equals: 'Change After Opening'
]

{ #category : #tests }
TabPresenterTest >> testSetOwner [
	"issue 16553"

	| button |
	button := ButtonPresenter new.
	self assert: button owner equals: nil.
	testedInstance presenter: button.
	self assert: button owner equals: testedInstance
]
