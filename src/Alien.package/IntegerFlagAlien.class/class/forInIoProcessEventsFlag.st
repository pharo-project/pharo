instance creation
forInIoProcessEventsFlag
	^self
		atAddress: self primInIOProcessEventsFlagAddress
		dataSize: 4