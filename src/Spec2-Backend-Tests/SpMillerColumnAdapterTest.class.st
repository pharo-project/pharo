Class {
	#name : #SpMillerColumnAdapterTest,
	#superclass : #SpAbstractWidgetAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #accessing }
SpMillerColumnAdapterTest >> classToTest [

	^ SpMillerColumnPresenter
]

{ #category : #running }
SpMillerColumnAdapterTest >> testChangingHelpAffectTheWidget [

	"This test is not valid for non-widget presenters"
	^ self skip
]

{ #category : #tests }
SpMillerColumnAdapterTest >> testEmptyMillerListHasNoWidgets [

	self assert: self adapter childrenWidgets isEmpty
]

{ #category : #running }
SpMillerColumnAdapterTest >> testEnabledAffectWidget [

	"This test is not valid for non-widget presenters"
	^ self skip
]

{ #category : #tests }
SpMillerColumnAdapterTest >> testManyPresentersShowManyWidgets [

	3 timesRepeat: [ self presenter addPresenter: (SpNullMillerPresenter on: SpLabelPresenter new).].
	self assert: self adapter childrenWidgets size equals: 3
]

{ #category : #tests }
SpMillerColumnAdapterTest >> testOnePresenterShowsOneWidget [

	self presenter addPresenter: (SpNullMillerPresenter on: SpLabelPresenter new).
	
	self assert: self adapter childrenWidgets size equals: 1
]

{ #category : #tests }
SpMillerColumnAdapterTest >> testResettingMillerListRemovesWidgets [

	3 timesRepeat: [ self presenter addPresenter: (SpNullMillerPresenter on: SpLabelPresenter new).].
	
	self presenter resetTo: 0.
	self assert: self adapter childrenWidgets isEmpty
]
