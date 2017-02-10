private
newItemFor: reference ifAbsent: failureBlock

	^ self log
		entryFor: reference
		ifPresent: [ :anEntry | 
			self 
				filter: anEntry 
				ifAccepted: [ self newEntryItemFor: anEntry ]
				ifNotAccepted: [ self newFilteredItemFor: anEntry ] ]
		ifAbsent: failureBlock
