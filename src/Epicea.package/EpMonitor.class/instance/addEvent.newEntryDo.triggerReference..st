private
addEvent: anEvent newEntryDo: aBlock triggerReference: triggerReference

	| newEntry |
	newEntry := log 
		addEntryWith: anEvent
		tags: [ :tags |
			triggerReference isNull ifFalse: [
				tags 
					at: EpLog triggererReferenceKey
					put: triggerReference ].
			
			tags at: EpLog authorKey put: self author.
			tags at: EpLog timeKey put: self time ].
		
	aBlock value: newEntry.