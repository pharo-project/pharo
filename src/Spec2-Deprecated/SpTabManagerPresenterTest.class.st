Class {
	#name : #SpTabManagerPresenterTest,
	#superclass : #SpSmokeTest,
	#instVars : [
		'redTab'
	],
	#category : #'Spec2-Deprecated-Tests'
}

{ #category : #deprecation }
SpTabManagerPresenterTest class >> isDeprecated [
	^ true
]

{ #category : #running }
SpTabManagerPresenterTest >> classToTest [
	^ SpTabManagerPresenter
]

{ #category : #running }
SpTabManagerPresenterTest >> initializeTestedInstance [
	redTab := self redMorphTab
]

{ #category : #utilities }
SpTabManagerPresenterTest >> morphicTabsFor: aManagerModel [
	"Retrieve all tabs in the morphic world to test whether they are being properly added/removed. It is assumed that the Model has already been built."

	^ (aManagerModel adapter widget submorphs
		detect: [ :each | each class = TabBarMorph ]) tabs
]

{ #category : #'tabs creation' }
SpTabManagerPresenterTest >> redMorphTab [
	| tab |
	tab := SpTabPresenter new.
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
SpTabManagerPresenterTest >> testAddTabOpened [
	self openInstance.
	self assertCollection: presenter tabs hasSameElements: #().
	presenter addTab: redTab.
	self assertCollection: presenter tabs hasSameElements: {redTab}
]

{ #category : #tests }
SpTabManagerPresenterTest >> testAddTabSetOwner [
	"issue #16553"

	self assert: redTab owner isNil.
	presenter addTab: redTab.
	self assert: redTab owner equals: presenter
]

{ #category : #tests }
SpTabManagerPresenterTest >> testAddTabUnopened [
	self assertCollection: presenter tabs hasSameElements: #().
	presenter addTab: redTab.
	self assertCollection: presenter tabs hasSameElements: {redTab}
]

{ #category : #tests }
SpTabManagerPresenterTest >> testInitialSelectedTab [
	"issue 15145"

	self assert: presenter selectedTab equals: nil.
	presenter addTab: redTab.
	self assert: presenter selectedTab equals: redTab.
	presenter addTab: self redMorphTab.
	self assert: presenter selectedTab equals: redTab
]

{ #category : #tests }
SpTabManagerPresenterTest >> testRemoveTab [
	"issue 11461"

	presenter addTab: redTab.
	self assertCollection: presenter tabs hasSameElements: {redTab}.
	presenter removeTab: redTab.
	self assertCollection: presenter tabs hasSameElements: #()
]

{ #category : #tests }
SpTabManagerPresenterTest >> testRemoveTabFromManager [
	"issue 11461 - update tab collection in the model when tab has been removed in morphic"

	self openInstance.
	presenter addTab: redTab.
	self assertCollection: presenter tabs hasSameElements: {redTab}.
	self
		assertCollection: (self morphicTabsFor: presenter)
		hasSameElements: {redTab adapter widget}.
	redTab adapter widget delete.
	self assertCollection: presenter tabs hasSameElements: #().
	self
		assertCollection: (self morphicTabsFor: presenter)
		hasSameElements: #()
]

{ #category : #tests }
SpTabManagerPresenterTest >> testRemoveTabFromManager2 [
	"issue 11461 -	update tab collection in the model when tab has been removed in morphic"

	presenter addTab: redTab.
	self assertCollection: presenter tabs hasSameElements: {redTab}.
	self openInstance.
	redTab adapter widget delete.
	self assertCollection: presenter tabs hasSameElements: #().
	self
		assertCollection: (self morphicTabsFor: presenter)
		hasSameElements: #()
]

{ #category : #tests }
SpTabManagerPresenterTest >> testRemoveTabFromMorphic [
	"issue 11461 - remove tab from morphic when it was removed from model"

	presenter addTab: redTab.
	self openInstance.
	self assertCollection: presenter tabs hasSameElements: {redTab}.
	presenter removeTab: redTab.
	self assertCollection: presenter tabs hasSameElements: #().
	self assertCollection: (self morphicTabsFor: presenter) hasSameElements: #()
]

{ #category : #tests }
SpTabManagerPresenterTest >> testRemoveTabUnsetOwner [
	"issue 16553"

	presenter addTab: redTab.
	self assert: redTab owner equals: presenter.
	presenter removeTab: redTab.
	self assert: redTab owner isNil 
]
