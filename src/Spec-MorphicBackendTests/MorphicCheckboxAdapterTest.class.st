Class {
	#name : #MorphicCheckboxAdapterTest,
	#superclass : #MorphicAdapterTestCase,
	#category : #'Spec-MorphicBackendTests'
}

{ #category : #running }
MorphicCheckboxAdapterTest >> classToTest [

	^ CheckBoxPresenter
]

{ #category : #utils }
MorphicCheckboxAdapterTest >> emulateCenterClick [

	self emulateClick: MouseButtonEvent blueButton
]

{ #category : #utils }
MorphicCheckboxAdapterTest >> emulateClick: mouseButtonCode [

	"Force opening the spec instance here.
	The action should have been correctly configured before
	depending on the spec initialization strategy"
	self openInstance.
	presenter adapter widget toggleSelected
]

{ #category : #utils }
MorphicCheckboxAdapterTest >> emulateLeftClick [

	self emulateClick: MouseButtonEvent redButton
]

{ #category : #utils }
MorphicCheckboxAdapterTest >> emulateRightClick [

	self emulateClick: MouseButtonEvent yellowButton
]

{ #category : #running }
MorphicCheckboxAdapterTest >> testActivatedCheckboxPresenterStartsActivatedCheckboxMorph [

	presenter state: true.
	self assert: self widget buttonMorph state equals: #on
]

{ #category : #running }
MorphicCheckboxAdapterTest >> testChangeActivatedAfterOpenCheckboxDectivatesIt [

	presenter state: false.
	
	"Force opening the spec instance here.
	We want to test that once open we can change the state"
	self openInstance.

	presenter state: true.
	self assert: presenter state
]

{ #category : #running }
MorphicCheckboxAdapterTest >> testChangeDeactivatedAfterOpenCheckboxDectivatesIt [

	presenter state: true.
	
	"Force opening the spec instance here.
	We want to test that once open we can change the state"
	self openInstance.
	
	presenter state: false.
	self deny: presenter state
]

{ #category : #running }
MorphicCheckboxAdapterTest >> testChangingLabelAffectTheWidget [
	
	presenter label: 'ALabel'.
	self assert: self widget label equals: 'ALabel'
]

{ #category : #running }
MorphicCheckboxAdapterTest >> testClickActivatedCheckboxDectivatesIt [

	presenter state: true.
	
	self emulateLeftClick.
	self deny: presenter state
]

{ #category : #running }
MorphicCheckboxAdapterTest >> testClickDeactivatedCheckboxActivatesIt [

	presenter state: false.
	
	self emulateLeftClick.
	self assert: presenter state
]

{ #category : #running }
MorphicCheckboxAdapterTest >> testClickRaisesChangedEvent [

	| activated |
	activated := false.
	presenter whenChangedDo: [ activated := true ].

	self emulateLeftClick.
	self assert: activated
]

{ #category : #running }
MorphicCheckboxAdapterTest >> testDeactivatedCheckboxPresenterStartsDeactivatedCheckboxMorph [

	presenter state: false.
	self assert: self widget buttonMorph state equals: #off
]
