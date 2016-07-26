announcement handling
addEvent: anEvent triggerReference: triggerReference

	^ log 
		addEntryWith: anEvent
		tags: [ :tags |
			triggerReference isNull ifFalse: [
				tags 
					at: EpLog triggererReferenceKey
					put: triggerReference ].
			
			tags at: EpLog authorKey put: self author.
			tags at: EpLog timeKey put: self time ] 