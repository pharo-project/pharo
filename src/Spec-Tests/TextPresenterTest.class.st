Class {
	#name : #TextPresenterTest,
	#superclass : #SpecSmokeTestCase,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #running }
TextPresenterTest >> classToTest [
	^ TextPresenter
]

{ #category : #running }
TextPresenterTest >> initializationText [
	presenter text: 'Text for tests.'
]

{ #category : #tests }
TextPresenterTest >> testClearSelection [
	self initializationText.
	self openInstance.
	presenter setSelection: (1 to: 10).
	self assert: presenter getSelection equals: (1 to: 10).
	presenter clearSelection.
	self assert: presenter getSelection isEmpty
]

{ #category : #tests }
TextPresenterTest >> testContextMenu [
	| menu changed |
	
	self assert: presenter contextMenu isNil.
	menu := MenuPresenter new.
	changed := false.
	presenter whenMenuChangedDo: [ 
		changed := true. ].
	presenter contextMenu: menu.
	self assert: presenter contextMenu equals: menu.
	self assert: changed
]

{ #category : #tests }
TextPresenterTest >> testSelectAll [
	self initializationText.
	self openInstance.
	presenter selectAll.
	self assert: presenter getSelection equals: (1 to: 15)
]
