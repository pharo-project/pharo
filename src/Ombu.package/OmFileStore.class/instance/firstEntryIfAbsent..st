enumerating
firstEntryIfAbsent: absentBlock

	^ self fileReference exists 
		ifFalse: absentBlock
		ifTrue: [
			[ self nextEntryFromPosition: 0 ] 
				on: Error 
				do: absentBlock ]