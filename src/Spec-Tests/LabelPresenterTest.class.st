Class {
	#name : #LabelPresenterTest,
	#superclass : #SpecSmokeTestCase,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #running }
LabelPresenterTest >> classToTest [
	^ LabelPresenter
]

{ #category : #tests }
LabelPresenterTest >> testLabelChangeRaisesEvent [

	self
		assertEvent: #whenChangedDo:
		isRaisedInPresenter: presenter
		whenDoing: [ presenter label: 'test' ]
]

{ #category : #tests }
LabelPresenterTest >> testLabelChangeRaisesEventOnce [

	self
		assertWith: [ :times | times = 1 ]
		timesRaisedEvent: #whenChangedDo:
		inPresenter: presenter
		whenDoing: [ presenter label: 'test' ]
]

{ #category : #tests }
LabelPresenterTest >> testLabelIsSet [

	presenter label: 'test'.
	self assert: presenter label equals: 'test'
]
