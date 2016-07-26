instance creation
scheduledAt: scheduledTime receiver: aTarget selector: aSelector arguments: argArray
	^(self receiver: aTarget selector: aSelector arguments: argArray)
		scheduledTime: scheduledTime.