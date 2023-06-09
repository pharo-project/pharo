"
A trait mock with a complex slot to be used in tests
"
Trait {
	#name : #TMockWithComplexSlot,
	#instVars : [
		'#complexSlot => ObservableSlot'
	],
	#category : #'TraitsV2-Tests'
}

{ #category : #accessing }
TMockWithComplexSlot >> complexSlot [

	^ complexSlot
]

{ #category : #initialization }
TMockWithComplexSlot >> initialize [

	super initialize.
	self class initializeSlots: self
]
