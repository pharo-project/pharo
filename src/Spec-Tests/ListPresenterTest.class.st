Class {
	#name : #ListPresenterTest,
	#superclass : #AbstractListPresenterTest,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #'tests-header' }
ListPresenterTest >> classToTest [
	
	^ ListPresenter
]

{ #category : #'tests-header' }
ListPresenterTest >> testHideHeaderTitleUnsetsTitle [

	presenter
		headerTitle: 'title';
		hideHeaderTitle.
	
	self deny: presenter hasHeaderTitle
]

{ #category : #'tests-header' }
ListPresenterTest >> testSetHeaderTitleHasTitle [

	presenter headerTitle: 'title'.
	
	self assert: presenter hasHeaderTitle
]

{ #category : #'tests-header' }
ListPresenterTest >> testSetHeaderTitleSetsTitle [

	presenter headerTitle: 'title'.
	
	self assert: presenter headerTitle equals: 'title'
]
