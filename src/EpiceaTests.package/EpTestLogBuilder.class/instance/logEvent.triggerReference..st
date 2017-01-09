building
logEvent: anEvent triggerReference: triggerReference
	log 
		addEntryWith: anEvent 
		tags: [ :tags |
			tags at: EpLog authorKey put: 'CoolAuthor'.
			tags at: EpLog timeKey put: DateAndTime now.
						
			triggerReference isNull ifFalse: [
				tags 
					at: EpLog triggererReferenceKey
					put: triggerReference ] ]