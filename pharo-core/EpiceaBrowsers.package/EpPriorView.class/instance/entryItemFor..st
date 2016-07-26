private
entryItemFor: entryReference
	^ entryItemByEntryReferenceCache
		at: entryReference
		ifAbsentPut: [
			self log 
				entryFor: entryReference
				ifPresent: [ :anEntry | 
					self newItemFor: anEntry ]
				ifAbsent: [ 
					self newAbsentItemFor: entryReference in: rootItems ] ]