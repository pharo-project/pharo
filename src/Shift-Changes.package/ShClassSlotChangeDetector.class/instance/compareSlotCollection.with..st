initialization
compareSlotCollection: a with: b
	a with: b do: [ :aSlot :bSlot | 
			(aSlot name = bSlot name and: [ aSlot species = bSlot species ])
				ifFalse: [ ^ false ] ].

	^ true