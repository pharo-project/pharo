comparing
compareSlotCollection: a with: b
	(a size = b size) ifFalse:[^false].

	a with: b do: [ :aSlot :bSlot | 
			(aSlot name = bSlot name and: [ aSlot species = bSlot species ])
				ifFalse: [ ^ false ] ].

	^ true