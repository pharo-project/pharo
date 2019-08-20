Class {
	#name : #TabManagerPresenterTest,
	#superclass : #SpecTestCase,
	#instVars : [
		'redTab'
	],
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #running }
TabManagerPresenterTest >> classToTest [
	^ TabManagerPresenter
]

{ #category : #running }
TabManagerPresenterTest >> initializeTestedInstance [
	redTab := self redMorphTab
]

{ #category : #utilities }
TabManagerPresenterTest >> morphicTabsFor: aManagerModel [
	"Retrieve all tabs in the morphic world to test whether they are being properly added/removed. It is assumed that the Model has already been built."

	^ (aManagerModel widget widget submorphs detect: [ :each | each class = TabBarMorph ]) tabs
]

{ #category : #'tabs creation' }
TabManagerPresenterTest >> redMorphTab [
	| tab |
	tab := TabPresenter new.
	tab
		label: 'Red Morph';
		icon: (tab iconNamed: #smallBackIcon);
		contents:
			(Morph new
				color: Color red;
				yourself).
	^ tab
]

{ #category : #tests }
TabManagerPresenterTest >> testAddTabOpened [
	self openInstance.
	self assertCollection: testedInstance tabs hasSameElements: #().
	testedInstance addTab: redTab.
	self assertCollection: testedInstance tabs hasSameElements: {redTab}
]

{ #category : #tests }
TabManagerPresenterTest >> testAddTabSetOwner [
	"issue #16553"

	self assert: redTab owner equals: nil.
	testedInstance addTab: redTab.
	self assert: redTab owner equals: testedInstance
]

{ #category : #tests }
TabManagerPresenterTest >> testAddTabUnopened [
	self assertCollection: testedInstance tabs hasSameElements: #().
	testedInstance addTab: redTab.
	self assertCollection: testedInstance tabs hasSameElements: {redTab}
]

{ #category : #tests }
TabManagerPresenterTest >> testInitialSelectedTab [
	"issue 15145"

	self assert: testedInstance selectedTab equals: nil.
	testedInstance addTab: redTab.
	self assert: testedInstance selectedTab equals: redTab.
	testedInstance addTab: self redMorphTab.
	self assert: testedInstance selectedTab equals: redTab
]

{ #category : #tests }
TabManagerPresenterTest >> testRemoveTab [
	"issue 11461"

	testedInstance addTab: redTab.
	self assertCollection: testedInstance tabs hasSameElements: {redTab}.
	testedInstance removeTab: redTab.
	self assertCollection: testedInstance tabs hasSameElements: #()
]

{ #category : #tests }
TabManagerPresenterTest >> testRemoveTabFromManager [
	"issue 11461 - update tab collection in the model when tab has been removed in morphic"

	self openInstance.
	testedInstance addTab: redTab.
	self assertCollection: testedInstance tabs hasSameElements: {redTab}.
	self assertCollection: (self morphicTabsFor: testedInstance) hasSameElements: {redTab widget widget}.
	redTab widget widget delete.
	self assertCollection: testedInstance tabs hasSameElements: #().
	self assertCollection: (self morphicTabsFor: testedInstance) hasSameElements: #()
]

{ #category : #tests }
TabManagerPresenterTest >> testRemoveTabFromManager2 [
	"issue 11461 -	update tab collection in the model when tab has been removed in morphic"

	testedInstance addTab: redTab.
	self assertCollection: testedInstance tabs hasSameElements: {redTab}.
	self openInstance .
	redTab widget widget delete.
	self assertCollection: testedInstance tabs hasSameElements: #().
	self assertCollection: (self morphicTabsFor: testedInstance) hasSameElements: #()
]

{ #category : #tests }
TabManagerPresenterTest >> testRemoveTabFromMorphic [
	"issue 11461 - remove tab from morphic when it was removed from model"

	testedInstance addTab: redTab.
	self openInstance.
	self assertCollection: testedInstance tabs hasSameElements: {redTab}.
	testedInstance removeTab: redTab.
	self assertCollection: testedInstance tabs hasSameElements: #().
	self assertCollection: (self morphicTabsFor: testedInstance) hasSameElements: #()
]

{ #category : #tests }
TabManagerPresenterTest >> testRemoveTabUnsetOwner [
	"issue 16553"

	testedInstance addTab: redTab.
	self assert: redTab owner equals: testedInstance.
	testedInstance removeTab: redTab.
	self assert: redTab owner equals: nil
]
