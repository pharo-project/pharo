building
logEvent: anEvent triggerReference: triggerReference
	self log 
		addEntryWith: anEvent 
		tags: [ :tags |
			tags at: EpLog authorKey put: 'TestAuthor'.
			tags at: EpLog timeKey put: DateAndTime now.
						
			triggerReference isNull ifFalse: [
				tags 
					at: EpLog triggererReferenceKey
					put: triggerReference ] ].
	self log store flush.