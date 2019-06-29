Class {
	#name : #SpObservableSlotTest,
	#superclass : #TestCase,
	#instVars : [
		'point'
	],
	#category : #'Spec2-Tests-Observable'
}

{ #category : #running }
SpObservableSlotTest >> setUp [

	super setUp.
	point := SpObservablePoint new.
]

{ #category : #tests }
SpObservableSlotTest >> testChangeInInstanceVariableRaisesEventOnlyOnce [

	|  count |
	count := 0.
	point property: #x whenChangedDo: [ count := count + 1 ].

	point x: 17.

	self assert: count equals: 1.
]

{ #category : #tests }
SpObservableSlotTest >> testExplicitNotificationRaisesEventOnlyOnce [

	| count |
	count := 0.
	point property: #x whenChangedDo: [ count := count + 1 ].

	point notifyPropertyChanged: #x.

	self assert: count equals: 1.
]

{ #category : #tests }
SpObservableSlotTest >> testExplicitNotificationRaisesEventWithNewValue [

	| newValue |
	point x: 17.
	point property: #x whenChangedDo: [ :new | newValue := new ].

	point notifyPropertyChanged: #x.

	self assert: newValue equals: 17.
]

{ #category : #tests }
SpObservableSlotTest >> testExplicitNotifyUnexistentPropertyChangedRaisesError [

	self
		should: [ point notifyPropertyChanged: #z ]
		raise: SlotNotFound
]

{ #category : #tests }
SpObservableSlotTest >> testObservableSlotWorksAsNormalSlot [

	point x: 17.
	point y: 299.
	
	self assert: point x equals: 17.
	self assert: point y equals: 299.
]

{ #category : #tests }
SpObservableSlotTest >> testSubscribeBlockWithoutParametersIsCalled [

	| called |
	called := false.
	point property: #x whenChangedDo: [ called := true ].

	point x: 17.

	self assert: called.
]

{ #category : #tests }
SpObservableSlotTest >> testSubscribeToChangeRaisesEventWithNewValue [

	| newValue |
	point property: #x whenChangedDo: [ :new | newValue := new ].

	point x: 17.

	self assert: newValue equals: 17.
]

{ #category : #tests }
SpObservableSlotTest >> testSubscribeToExistentNonObservablePropertyRaisesError [

	self
		should: [
			point
				property: #y
				whenChangedDo: [ self fail: 'This event shouldnt have been subscribed at all' ] ]
		raise: SpNonObservableSlotError
]

{ #category : #tests }
SpObservableSlotTest >> testSubscribeToUnexistentPropertyRaisesError [

	self
		should: [
			point
				property: #z
				whenChangedDo: [ self fail: 'This event shouldnt have been subscribed at all' ] ]
		raise: SlotNotFound
]
