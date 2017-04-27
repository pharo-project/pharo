private
triggerItemsFor: anEntry

	^ self log 
		triggererReferenceOf: anEntry 
		ifPresent: [ :triggerEntryReference | 
			| triggerItem |
			triggerItem := self itemForEntryReference: triggerEntryReference.
			triggerItem triggerItems, { triggerItem } ]
		ifAbsent: [ #() ]
