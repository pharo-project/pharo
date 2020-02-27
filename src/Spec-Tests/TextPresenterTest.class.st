Class {
	#name : #TextPresenterTest,
	#superclass : #SpecTestCase,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #running }
TextPresenterTest >> classToTest [
	^ TextPresenter
]

{ #category : #running }
TextPresenterTest >> initializationText [
	testedInstance text: 'Text for tests.'
]

{ #category : #tests }
TextPresenterTest >> testSelectAll [
	self initializationText.
	self openInstance.
	testedInstance selectAll.
	self assert: testedInstance getSelection equals: (1 to: 15).
	testedInstance clearSelection.
	self assert: testedInstance getSelection isEmpty
]
