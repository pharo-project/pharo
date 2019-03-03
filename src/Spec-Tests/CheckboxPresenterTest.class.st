Class {
	#name : #CheckboxPresenterTest,
	#superclass : #SpecTestCase,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #running }
CheckboxPresenterTest >> classToTest [

	^ CheckBoxPresenter
]

{ #category : #running }
CheckboxPresenterTest >> morph [

	^ presenter adapter widget
]

{ #category : #tests }
CheckboxPresenterTest >> testActivateDoesNotRaiseDeactivateEvent [

	presenter state: false.
	self
		assertWith: [ :times | times = 1 ]
		timesRaisedEvent: #whenDeactivatedDo:
		inPresenter: presenter
		whenDoing: [ presenter state: true ]
]

{ #category : #tests }
CheckboxPresenterTest >> testActivatePresenterIsActive [

	presenter state: true.
	self assert: presenter state
]

{ #category : #tests }
CheckboxPresenterTest >> testActivateRaisesActivatedEvent [

	presenter state: false.
	self
		assertEvent: #whenActivatedDo:
		isRaisedInPresenter: presenter
		whenDoing: [ presenter state: true ]
]

{ #category : #tests }
CheckboxPresenterTest >> testActivateRaisesChangedEvent [

	presenter state: false.
	self
		assertEvent: #whenChangedDo:
		isRaisedInPresenter: presenter
		whenDoing: [ presenter state: true ]
]

{ #category : #tests }
CheckboxPresenterTest >> testActivateRaisesChangedEventOnce [

	presenter state: false.
	self
		assertWith: [ :times | times = 1 ]
		timesRaisedEvent: #whenChangedDo:
		inPresenter: presenter
		whenDoing: [ presenter state: true ]
]

{ #category : #tests }
CheckboxPresenterTest >> testDeactivateDoesNotRaiseActivateEvent [

	presenter state: true.
	self
		assertWith: [ :times | times = 1 ]
		timesRaisedEvent: #whenActivatedDo:
		inPresenter: presenter
		whenDoing: [ presenter state: false ]
]

{ #category : #tests }
CheckboxPresenterTest >> testDeactivatePresenterIsNotActive [

	presenter state: false.
	self deny: presenter state
]

{ #category : #tests }
CheckboxPresenterTest >> testDeactivateRaisesChangedEvent [

	presenter state: true.
	self
		assertEvent: #whenChangedDo:
		isRaisedInPresenter: presenter
		whenDoing: [ presenter state: false ]
]

{ #category : #tests }
CheckboxPresenterTest >> testDeactivateRaisesChangedEventOnce [

	presenter state: true.
	self
		assertWith: [ :times | times = 1 ]
		timesRaisedEvent: #whenChangedDo:
		inPresenter: presenter
		whenDoing: [ presenter state: false ]
]

{ #category : #tests }
CheckboxPresenterTest >> testDeactivateRaisesDeactivatedEvent [

	presenter state: true.
	self
		assertEvent: #whenDeactivatedDo:
		isRaisedInPresenter: presenter
		whenDoing: [ presenter state: false ]
]

{ #category : #tests }
CheckboxPresenterTest >> testLabelIsSet [

	presenter label: 'test'.
	self assert: presenter label equals: 'test'
]

{ #category : #tests }
CheckboxPresenterTest >> testStayingActiveDoesNotRaiseChangedEvent [

	presenter state: true.
	self
		denyEvent: #whenChangedDo:
		isRaisedInPresenter: presenter
		whenDoing: [ presenter state: true ]
]

{ #category : #tests }
CheckboxPresenterTest >> testStayingInactiveDoesNotRaiseChangedEvent [

	presenter state: false.
	self
		denyEvent: #whenChangedDo:
		isRaisedInPresenter: presenter
		whenDoing: [ presenter state: false ]
]
