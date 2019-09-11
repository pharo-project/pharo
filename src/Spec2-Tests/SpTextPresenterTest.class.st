Class {
	#name : #SpTextPresenterTest,
	#superclass : #SpSmokeTestCase,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpTextPresenterTest >> classToTest [
	^ SpTextPresenter
]

{ #category : #running }
SpTextPresenterTest >> initializationText [
	presenter text: 'Text for tests.'
]

{ #category : #tests }
SpTextPresenterTest >> testClearSelection [
	self initializationText.
	self openInstance.
	presenter setSelection: (1 to: 10).
	self assert: presenter getSelection equals: (1 to: 10).
	presenter clearSelection.
	self assert: presenter getSelection isEmpty
]

{ #category : #tests }
SpTextPresenterTest >> testContextMenu [
	| menu changed |
	
	self assert: presenter contextMenu isNil.
	menu := SpMenuPresenter new.
	changed := false.
	presenter whenMenuChangedDo: [ 
		changed := true. ].
	presenter contextMenu: menu.
	self assert: presenter contextMenu equals: menu.
	self assert: changed
]

{ #category : #tests }
SpTextPresenterTest >> testSelectAll [
	self initializationText.
	self openInstance.
	presenter selectAll.
	self assert: presenter getSelection equals: (1 to: 15)
]
