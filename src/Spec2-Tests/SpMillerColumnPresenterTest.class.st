Class {
	#name : #SpMillerColumnPresenterTest,
	#superclass : #SpSmokeTest,
	#category : #'Spec2-Tests-Miller'
}

{ #category : #running }
SpMillerColumnPresenterTest >> classToTest [
	
	^ SpMillerColumnPresenter
]

{ #category : #tests }
SpMillerColumnPresenterTest >> testActivateSubPresenterPushesElementToList [

	| mock |
	self presenter presenterBlock: [ :model | mock := SpMockMillerPresenter new ].	
	self presenter setRootModel: 1.
	
	mock activate.
	self assert: self presenter size equals: 2
]

{ #category : #tests }
SpMillerColumnPresenterTest >> testAddManyPresentersPushesThemToPresenterList [

	3 timesRepeat: [self presenter addPresenter: (SpNullMillerPresenter on: SpLabelPresenter new)].
	self assert: self presenter size equals: 3
]

{ #category : #tests }
SpMillerColumnPresenterTest >> testAddPresenterPushesItToPresenterList [

	self presenter addPresenter: (SpNullMillerPresenter on: SpLabelPresenter new).
	self assert: self presenter size equals: 1
]

{ #category : #tests }
SpMillerColumnPresenterTest >> testPushModelUsesPresenterBlock [

	| subpresenter |
	self presenter presenterBlock: [ :model | 
		subpresenter := SpNullMillerPresenter on: (SpLabelPresenter new label: model asString) ].	
	self presenter pushModel: 1.
	self assert: self presenter presenters first equals: subpresenter
]

{ #category : #tests }
SpMillerColumnPresenterTest >> testSetRootModelManyTimesPushesOnlyLastPresenterToList [

	| nullMillerPresenter |
	self presenter presenterBlock: [ :model | 
		SpNullMillerPresenter on: (SpLabelPresenter new label: model asString) ].

	1 to: 3 do: [ :each | self presenter setRootModel: each ].
	
	nullMillerPresenter := self presenter presenters first.
	self
		assert: nullMillerPresenter wrappedPresenter label
		equals: 3 asString
]

{ #category : #tests }
SpMillerColumnPresenterTest >> testSetRootModelManyTimesPushesOnlyOnePresenterToList [

	self presenter presenterBlock: [ :model | 
		SpNullMillerPresenter on: (SpLabelPresenter new label: model asString) ].

	1 to: 3 do: [ :each | self presenter setRootModel: each ].
	
	self assert: self presenter size equals: 1
]

{ #category : #tests }
SpMillerColumnPresenterTest >> testSetRootModelPushesPresenterToList [

	self presenter presenterBlock: [ :model | 
		SpNullMillerPresenter on: (SpLabelPresenter new label: model asString) ].
	self presenter setRootModel: 1.
	
	self assert: self presenter size equals: 1
]

{ #category : #tests }
SpMillerColumnPresenterTest >> testSetRootModelResetsToSinglePresenter [

	self presenter presenterBlock: [ :model | 
		SpNullMillerPresenter on: (SpLabelPresenter new label: model asString) ].
	3 timesRepeat: [self presenter addPresenter: (SpNullMillerPresenter on: SpLabelPresenter new)].
	
	self presenter setRootModel: 1.
	self assert: self presenter size equals: 1
]

{ #category : #tests }
SpMillerColumnPresenterTest >> testSetRootModelWithPresenterDoesNotFail [

	self presenter presenterBlock: [ :model | 
		SpNullMillerPresenter on: (SpLabelPresenter new label: model asString) ].
	
	self
		shouldnt: [self presenter setRootModel: 1]
		raise: Error
]

{ #category : #tests }
SpMillerColumnPresenterTest >> testSetRootModelWithoutPresenterBlockFails [

	self
		should: [self presenter setRootModel: 1]
		raise: Error
]
