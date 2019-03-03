Class {
	#name : #SpecObservableSlotTest,
	#superclass : #TestCase,
	#instVars : [
		'point'
	],
	#category : #'Spec-Tests-Observable'
}

{ #category : #running }
SpecObservableSlotTest >> setUp [

	super setUp.
	point := SpecObservablePoint new.
]

{ #category : #tests }
SpecObservableSlotTest >> testChangeInInstanceVariableRaisesEventOnlyOnce [

	|  count |
	count := 0.
	point property: #x whenChangedDo: [ count := count + 1 ].

	point x: 17.

	self assert: count equals: 1.
]

{ #category : #tests }
SpecObservableSlotTest >> testExplicitNotificationRaisesEventOnlyOnce [

	| count |
	count := 0.
	point property: #x whenChangedDo: [ count := count + 1 ].

	point notifyPropertyChanged: #x.

	self assert: count equals: 1.
]

{ #category : #tests }
SpecObservableSlotTest >> testExplicitNotificationRaisesEventWithNewValue [

	| newValue |
	point x: 17.
	point property: #x whenChangedDo: [ :new | newValue := new ].

	point notifyPropertyChanged: #x.

	self assert: newValue equals: 17.
]

{ #category : #tests }
SpecObservableSlotTest >> testExplicitNotifyUnexistentPropertyChangedRaisesError [

	self
		should: [ point notifyPropertyChanged: #z ]
		raise: SlotNotFound
]

{ #category : #tests }
SpecObservableSlotTest >> testObservableSlotWorksAsNormalSlot [

	point x: 17.
	point y: 299.
	
	self assert: point x equals: 17.
	self assert: point y equals: 299.
]

{ #category : #tests }
SpecObservableSlotTest >> testSubscribeBlockWithoutParametersIsCalled [

	| called |
	called := false.
	point property: #x whenChangedDo: [ called := true ].

	point x: 17.

	self assert: called.
]

{ #category : #tests }
SpecObservableSlotTest >> testSubscribeToChangeRaisesEventWithNewValue [

	| newValue |
	point property: #x whenChangedDo: [ :new | newValue := new ].

	point x: 17.

	self assert: newValue equals: 17.
]

{ #category : #tests }
SpecObservableSlotTest >> testSubscribeToExistentNonObservablePropertyRaisesError [

	self
		should: [
			point
				property: #y
				whenChangedDo: [ self fail: 'This event shouldnt have been subscribed at all' ] ]
		raise: SpecNonObservableSlotError
]

{ #category : #tests }
SpecObservableSlotTest >> testSubscribeToUnexistentPropertyRaisesError [

	self
		should: [
			point
				property: #z
				whenChangedDo: [ self fail: 'This event shouldnt have been subscribed at all' ] ]
		raise: SlotNotFound
]
