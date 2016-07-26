private
triggerItemsFor: anEntry
	^ self log 
		triggererReferenceOf: anEntry 
		ifPresent: [ :triggerEntryReference | 
			| triggerItem |
			triggerItem := self entryItemFor: triggerEntryReference.
			triggerItem triggerItems, { triggerItem } ]
		ifAbsent: [ #() ]
	