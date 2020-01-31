Class {
	#name : #SpTextPresenterTest,
	#superclass : #SpAbstractTextPresenterTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpTextPresenterTest >> classToTest [
	^ SpTextPresenter
]

{ #category : #tests }
SpTextPresenterTest >> testInsertAt [
	
	self initializationText.
	self openInstance.
	presenter insert: 'insertion ' at: 9.
	self assert: presenter text equals: 'Text for insertion tests.'
]

{ #category : #tests }
SpTextPresenterTest >> testSelectLine [
	self initializationText.
	self openInstance.
	presenter selectLine.
	self assert: presenter selectionInterval equals: (1 to: 15)
]
