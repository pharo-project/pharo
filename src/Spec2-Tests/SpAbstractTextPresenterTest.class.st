Class {
	#name : #SpAbstractTextPresenterTest,
	#superclass : #SpSmokeTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #testing }
SpAbstractTextPresenterTest class >> isAbstract [
	^ self = SpAbstractTextPresenterTest
]

{ #category : #testing }
SpAbstractTextPresenterTest class >> shouldInheritSelectors [
	^ true
]

{ #category : #running }
SpAbstractTextPresenterTest >> classToTest [
	^ self subclassResponsibility
]

{ #category : #running }
SpAbstractTextPresenterTest >> initializationText [
	presenter text: 'Text for tests.'
]

{ #category : #tests }
SpAbstractTextPresenterTest >> testClearContent [
	self initializationText.
	self denyEmpty: presenter text.
	presenter clearContent.
	self assertEmpty: presenter text
]

{ #category : #tests }
SpAbstractTextPresenterTest >> testClearSelection [
	self initializationText.
	self openInstance.
	presenter selectionInterval: (1 to: 10).
	self assert: presenter selectionInterval equals: (1 to: 10).
	presenter clearSelection.
	self assert: presenter selectionInterval isEmpty
]

{ #category : #tests }
SpAbstractTextPresenterTest >> testContextMenu [
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
SpAbstractTextPresenterTest >> testPlaceholderIsSet [

	presenter placeholder: 'enter something...'.
	
	self assert: presenter placeholder equals: 'enter something...'
]

{ #category : #tests }
SpAbstractTextPresenterTest >> testSelectAll [
	self initializationText.
	self openInstance.
	presenter selectAll.
	self assert: presenter selectionInterval equals: (1 to: 15)
]
