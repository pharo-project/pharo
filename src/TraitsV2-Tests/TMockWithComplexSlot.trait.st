Trait {
	#name : #TMockWithComplexSlot,
	#instVars : [
		'#aComplexSlot => ObservableSlot'
	],
	#category : #'TraitsV2-Tests'
}

{ #category : #initialization }
TMockWithComplexSlot >> initialize [
	
	self class initializeSlots: self.
	super initialize.
]
