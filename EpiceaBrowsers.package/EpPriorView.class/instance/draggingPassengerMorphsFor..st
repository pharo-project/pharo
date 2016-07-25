private
draggingPassengerMorphsFor: items

	| morphs upperBound |
	upperBound := 5.

	morphs := items collect: #displayWidget.	

	items size > upperBound ifTrue: [ 
		morphs := (morphs first: upperBound) copyWith: '...' asMorph ].

	^ morphs 