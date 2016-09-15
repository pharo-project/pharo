private
entryItemFor: entryReference
	^ entryItemByEntryReferenceCache
		at: entryReference
		ifAbsentPut: [
			self log 
				entryFor: entryReference
				ifPresent: [ :anEntry | 
					self newEntryItemFor: anEntry ]
				ifAbsent: [ 
					self newAbsentItemFor: entryReference in: rootItems ] ]