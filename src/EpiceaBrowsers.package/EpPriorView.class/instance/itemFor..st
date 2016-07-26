private
itemFor: entryReference
	^ self log 
		entryFor: entryReference
		ifPresent: [ :anEntry | 
			self 
				filter: anEntry 
				ifAccepted: [ self newItemFor: anEntry ]
				ifNotAccepted: [ self newFilteredItemFor: anEntry ] ]
		ifAbsent: [ 
			self newAbsentItemFor: entryReference in: rootItems ]