Class {
	#name : #CheckboxAdapterTest,
	#superclass : #AbstractWidgetAdapterTestCase,
	#category : #'Spec-BackendTests'
}

{ #category : #running }
CheckboxAdapterTest >> classToTest [

	^ CheckBoxPresenter
]

{ #category : #running }
CheckboxAdapterTest >> testActivatedCheckboxPresenterStartsActivatedCheckboxMorph [

	presenter state: true.
	self assert: self adapter state
]

{ #category : #running }
CheckboxAdapterTest >> testChangeActivatedAfterOpenCheckboxDectivatesIt [

	presenter state: false.
	
	"Force opening the spec instance here.
	We want to test that once open we can change the state"
	self openInstance.

	presenter state: true.
	self assert: presenter state
]

{ #category : #running }
CheckboxAdapterTest >> testChangeDeactivatedAfterOpenCheckboxDectivatesIt [

	presenter state: true.
	
	"Force opening the spec instance here.
	We want to test that once open we can change the state"
	self openInstance.
	
	presenter state: false.
	self deny: presenter state
]

{ #category : #running }
CheckboxAdapterTest >> testChangingLabelAffectTheWidget [
	
	presenter label: 'ALabel'.
	self assert: self widget label equals: 'ALabel'
]

{ #category : #running }
CheckboxAdapterTest >> testClickActivatedCheckboxDectivatesIt [

	presenter state: true.
	
	self emulateLeftClick.
	self deny: presenter state
]

{ #category : #running }
CheckboxAdapterTest >> testClickDeactivatedCheckboxActivatesIt [

	presenter state: false.
	
	self emulateLeftClick.
	self assert: presenter state
]

{ #category : #running }
CheckboxAdapterTest >> testClickRaisesChangedEvent [

	| activated |
	activated := false.
	presenter whenChangedDo: [ activated := true ].

	self emulateLeftClick.
	self assert: activated
]

{ #category : #running }
CheckboxAdapterTest >> testDeactivatedCheckboxPresenterStartsDeactivatedCheckboxMorph [

	presenter state: false.
	self deny: self adapter state
]
