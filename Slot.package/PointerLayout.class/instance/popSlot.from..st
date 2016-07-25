diff
popSlot: aSlot from: collection
	collection withIndexDo: [ :slot :index |
		slot name == aSlot name
			ifTrue: [ ^ collection removeAt: index ]].
	^ nil