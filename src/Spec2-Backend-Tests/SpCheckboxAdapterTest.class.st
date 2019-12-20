Class {
	#name : #SpCheckboxAdapterTest,
	#superclass : #SpAbstractWidgetAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #running }
SpCheckboxAdapterTest >> classToTest [

	^ SpCheckBoxPresenter
]

{ #category : #running }
SpCheckboxAdapterTest >> testActivatedCheckboxPresenterStartsActivatedCheckboxMorph [

	presenter state: true.
	self assert: self adapter state
]

{ #category : #running }
SpCheckboxAdapterTest >> testChangeActivatedAfterOpenCheckboxDectivatesIt [

	presenter state: false.
	
	"Force opening the spec instance here.
	We want to test that once open we can change the state"
	self openInstance.

	presenter state: true.
	self assert: presenter state
]

{ #category : #running }
SpCheckboxAdapterTest >> testChangeDeactivatedAfterOpenCheckboxDectivatesIt [

	presenter state: true.
	
	"Force opening the spec instance here.
	We want to test that once open we can change the state"
	self openInstance.
	
	presenter state: false.
	self deny: presenter state
]

{ #category : #running }
SpCheckboxAdapterTest >> testChangingLabelAffectTheWidget [
	
	presenter label: 'ALabel'.
	self assert: self widget label equals: 'ALabel'
]

{ #category : #running }
SpCheckboxAdapterTest >> testClickActivatedCheckboxDectivatesIt [

	presenter state: true.
	
	self emulateLeftClick.
	self deny: presenter state
]

{ #category : #running }
SpCheckboxAdapterTest >> testClickDeactivatedCheckboxActivatesIt [

	presenter state: false.
	
	self emulateLeftClick.
	self assert: presenter state
]

{ #category : #running }
SpCheckboxAdapterTest >> testClickRaisesChangedEvent [

	| activated |
	activated := false.
	presenter whenChangedDo: [ activated := true ].

	self emulateLeftClick.
	self assert: activated
]

{ #category : #running }
SpCheckboxAdapterTest >> testDeactivatedCheckboxPresenterStartsDeactivatedCheckboxMorph [

	presenter state: false.
	self deny: self adapter state
]
