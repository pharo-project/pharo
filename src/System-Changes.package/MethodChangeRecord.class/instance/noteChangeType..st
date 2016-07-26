all changes
noteChangeType: newChangeType
	changeType := (changeType == #addedThenRemoved and: [ newChangeType == #change ])
		ifTrue: [ #add ]
		ifFalse: [ newChangeType ]