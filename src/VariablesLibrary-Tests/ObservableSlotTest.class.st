Class {
	#name : #ObservableSlotTest,
	#superclass : #TestCase,
	#instVars : [
		'point'
	],
	#category : #'VariablesLibrary-Tests-Observable'
}

{ #category : #running }
ObservableSlotTest >> setUp [
	super setUp.
	point := ObservablePoint new
]

{ #category : #tests }
ObservableSlotTest >> testChangeInInstanceVariableRaisesEventOnlyOnce [
	| count |
	count := 0.
	point property: #x whenChangedDo: [ count := count + 1 ].

	point x: 17.

	self assert: count equals: 1
]

{ #category : #tests }
ObservableSlotTest >> testExplicitNotificationRaisesEventOnlyOnce [
	| count |
	count := 0.
	point property: #x whenChangedDo: [ count := count + 1 ].

	point notifyPropertyChanged: #x.

	self assert: count equals: 1
]

{ #category : #tests }
ObservableSlotTest >> testExplicitNotificationRaisesEventWithNewValue [
	| newValue |
	point x: 17.
	point property: #x whenChangedDo: [ :new | newValue := new ].

	point notifyPropertyChanged: #x.

	self assert: newValue equals: 17
]

{ #category : #tests }
ObservableSlotTest >> testExplicitNotifyUnexistentPropertyChangedRaisesError [
	self should: [ point notifyPropertyChanged: #z ] raise: SlotNotFound
]

{ #category : #tests }
ObservableSlotTest >> testObservableSlotWorksAsNormalSlot [
	point x: 17.
	point y: 299.

	self assert: point x equals: 17.
	self assert: point y equals: 299
]

{ #category : #tests }
ObservableSlotTest >> testSubscribeBlockWithoutParametersIsCalled [
	| called |
	called := false.
	point property: #x whenChangedDo: [ called := true ].

	point x: 17.

	self assert: called
]

{ #category : #tests }
ObservableSlotTest >> testSubscribeToChangeRaisesEventWithNewValue [
	| newValue |
	point property: #x whenChangedDo: [ :new | newValue := new ].

	point x: 17.

	self assert: newValue equals: 17
]

{ #category : #tests }
ObservableSlotTest >> testSubscribeToExistentNonObservablePropertyRaisesError [

	self
		should: [
			point
				property: #y
				whenChangedDo: [ self fail: 'This event shouldnt have been subscribed at all' ] ]
		raise: NonObservableSlotError
]

{ #category : #tests }
ObservableSlotTest >> testSubscribeToUnexistentPropertyRaisesError [

	self
		should: [
			point
				property: #z
				whenChangedDo: [ self fail: 'This event shouldnt have been subscribed at all' ] ]
		raise: SlotNotFound
]
