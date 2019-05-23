Class {
	#name : #MillerColumnPresenterTest,
	#superclass : #SpecSmokeTestCase,
	#category : #'Spec-Tests-Miller'
}

{ #category : #running }
MillerColumnPresenterTest >> classToTest [
	
	^ SpecMillerColumnPresenter
]

{ #category : #tests }
MillerColumnPresenterTest >> testActivateSubPresenterPushesElementToList [

	| mock |
	self presenter presenterBlock: [ :model | mock := SpecMockMillerPresenter new ].	
	self presenter setRootModel: 1.
	
	mock activate.
	self assert: self presenter size equals: 2
]

{ #category : #tests }
MillerColumnPresenterTest >> testAddManyPresentersPushesThemToPresenterList [

	3 timesRepeat: [self presenter addPresenter: (SpecNullMillerPresenter on: LabelPresenter new)].
	self assert: self presenter size equals: 3
]

{ #category : #tests }
MillerColumnPresenterTest >> testAddPresenterPushesItToPresenterList [

	self presenter addPresenter: (SpecNullMillerPresenter on: LabelPresenter new).
	self assert: self presenter size equals: 1
]

{ #category : #tests }
MillerColumnPresenterTest >> testPushModelUsesPresenterBlock [

	| subpresenter |
	self presenter presenterBlock: [ :model | 
		subpresenter := SpecNullMillerPresenter on: (LabelPresenter new label: model asString) ].	
	self presenter pushModel: 1.
	self assert: self presenter presenters first equals: subpresenter
]

{ #category : #tests }
MillerColumnPresenterTest >> testSetRootModelManyTimesPushesOnlyLastPresenterToList [

	| nullMillerPresenter |
	self presenter presenterBlock: [ :model | 
		SpecNullMillerPresenter on: (LabelPresenter new label: model asString) ].

	1 to: 3 do: [ :each | self presenter setRootModel: each ].
	
	nullMillerPresenter := self presenter presenters first.
	self
		assert: nullMillerPresenter wrappedPresenter label
		equals: 3 asString
]

{ #category : #tests }
MillerColumnPresenterTest >> testSetRootModelManyTimesPushesOnlyOnePresenterToList [

	self presenter presenterBlock: [ :model | 
		SpecNullMillerPresenter on: (LabelPresenter new label: model asString) ].

	1 to: 3 do: [ :each | self presenter setRootModel: each ].
	
	self assert: self presenter size equals: 1
]

{ #category : #tests }
MillerColumnPresenterTest >> testSetRootModelPushesPresenterToList [

	self presenter presenterBlock: [ :model | 
		SpecNullMillerPresenter on: (LabelPresenter new label: model asString) ].
	self presenter setRootModel: 1.
	
	self assert: self presenter size equals: 1
]

{ #category : #tests }
MillerColumnPresenterTest >> testSetRootModelResetsToSinglePresenter [

	self presenter presenterBlock: [ :model | 
		SpecNullMillerPresenter on: (LabelPresenter new label: model asString) ].
	3 timesRepeat: [self presenter addPresenter: (SpecNullMillerPresenter on: LabelPresenter new)].
	
	self presenter setRootModel: 1.
	self assert: self presenter size equals: 1
]

{ #category : #tests }
MillerColumnPresenterTest >> testSetRootModelWithPresenterDoesNotFail [

	self presenter presenterBlock: [ :model | 
		SpecNullMillerPresenter on: (LabelPresenter new label: model asString) ].
	
	self
		shouldnt: [self presenter setRootModel: 1]
		raise: Error
]

{ #category : #tests }
MillerColumnPresenterTest >> testSetRootModelWithoutPresenterBlockFails [

	self
		should: [self presenter setRootModel: 1]
		raise: Error
]
