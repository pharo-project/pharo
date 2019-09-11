Class {
	#name : #MillerColumnAdapterTest,
	#superclass : #AbstractWidgetAdapterTestCase,
	#category : #'Spec-BackendTests'
}

{ #category : #accessing }
MillerColumnAdapterTest >> classToTest [

	^ SpecMillerColumnPresenter
]

{ #category : #running }
MillerColumnAdapterTest >> testChangingHelpAffectTheWidget [

	"This test is not valid for non-widget presenters"
	^ self skip
]

{ #category : #tests }
MillerColumnAdapterTest >> testEmptyMillerListHasNoWidgets [

	self assert: self adapter childrenWidgets isEmpty
]

{ #category : #running }
MillerColumnAdapterTest >> testEnabledAffectWidget [

	"This test is not valid for non-widget presenters"
	^ self skip
]

{ #category : #tests }
MillerColumnAdapterTest >> testManyPresentersShowManyWidgets [

	3 timesRepeat: [ self presenter addPresenter: (SpecNullMillerPresenter on: LabelPresenter new).].
	self assert: self adapter childrenWidgets size equals: 3
]

{ #category : #tests }
MillerColumnAdapterTest >> testOnePresenterShowsOneWidget [

	self presenter addPresenter: (SpecNullMillerPresenter on: LabelPresenter new).
	
	self assert: self adapter childrenWidgets size equals: 1
]

{ #category : #tests }
MillerColumnAdapterTest >> testResettingMillerListRemovesWidgets [

	3 timesRepeat: [ self presenter addPresenter: (SpecNullMillerPresenter on: LabelPresenter new).].
	
	self presenter resetTo: 0.
	self assert: self adapter childrenWidgets isEmpty
]
