Class {
	#name : #SpCheckboxPresenterTest,
	#superclass : #SpTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpCheckboxPresenterTest >> classToTest [

	^ SpCheckBoxPresenter
]

{ #category : #running }
SpCheckboxPresenterTest >> morph [

	^ presenter adapter widget
]

{ #category : #tests }
SpCheckboxPresenterTest >> testActivateDoesNotRaiseDeactivateEvent [

	presenter state: false.
	self
		assertWith: [ :times | times = 1 ]
		timesRaisedEvent: #whenDeactivatedDo:
		inPresenter: presenter
		whenDoing: [ presenter state: true ]
]

{ #category : #tests }
SpCheckboxPresenterTest >> testActivatePresenterIsActive [

	presenter state: true.
	self assert: presenter state
]

{ #category : #tests }
SpCheckboxPresenterTest >> testActivateRaisesActivatedEvent [

	presenter state: false.
	self
		assertEvent: #whenActivatedDo:
		isRaisedInPresenter: presenter
		whenDoing: [ presenter state: true ]
]

{ #category : #tests }
SpCheckboxPresenterTest >> testActivateRaisesChangedEvent [

	presenter state: false.
	self
		assertEvent: #whenChangedDo:
		isRaisedInPresenter: presenter
		whenDoing: [ presenter state: true ]
]

{ #category : #tests }
SpCheckboxPresenterTest >> testActivateRaisesChangedEventOnce [

	presenter state: false.
	self
		assertWith: [ :times | times = 1 ]
		timesRaisedEvent: #whenChangedDo:
		inPresenter: presenter
		whenDoing: [ presenter state: true ]
]

{ #category : #tests }
SpCheckboxPresenterTest >> testDeactivateDoesNotRaiseActivateEvent [

	presenter state: true.
	self
		assertWith: [ :times | times = 1 ]
		timesRaisedEvent: #whenActivatedDo:
		inPresenter: presenter
		whenDoing: [ presenter state: false ]
]

{ #category : #tests }
SpCheckboxPresenterTest >> testDeactivatePresenterIsNotActive [

	presenter state: false.
	self deny: presenter state
]

{ #category : #tests }
SpCheckboxPresenterTest >> testDeactivateRaisesChangedEvent [

	presenter state: true.
	self
		assertEvent: #whenChangedDo:
		isRaisedInPresenter: presenter
		whenDoing: [ presenter state: false ]
]

{ #category : #tests }
SpCheckboxPresenterTest >> testDeactivateRaisesChangedEventOnce [

	presenter state: true.
	self
		assertWith: [ :times | times = 1 ]
		timesRaisedEvent: #whenChangedDo:
		inPresenter: presenter
		whenDoing: [ presenter state: false ]
]

{ #category : #tests }
SpCheckboxPresenterTest >> testDeactivateRaisesDeactivatedEvent [

	presenter state: true.
	self
		assertEvent: #whenDeactivatedDo:
		isRaisedInPresenter: presenter
		whenDoing: [ presenter state: false ]
]

{ #category : #tests }
SpCheckboxPresenterTest >> testLabelIsSet [

	presenter label: 'test'.
	self assert: presenter label equals: 'test'
]

{ #category : #tests }
SpCheckboxPresenterTest >> testStayingActiveDoesNotRaiseChangedEvent [

	presenter state: true.
	self
		denyEvent: #whenChangedDo:
		isRaisedInPresenter: presenter
		whenDoing: [ presenter state: true ]
]

{ #category : #tests }
SpCheckboxPresenterTest >> testStayingInactiveDoesNotRaiseChangedEvent [

	presenter state: false.
	self
		denyEvent: #whenChangedDo:
		isRaisedInPresenter: presenter
		whenDoing: [ presenter state: false ]
]
