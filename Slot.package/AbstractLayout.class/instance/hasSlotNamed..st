api
hasSlotNamed: aString
	^self allSlots anySatisfy: [:slot | slot name = aString  ]