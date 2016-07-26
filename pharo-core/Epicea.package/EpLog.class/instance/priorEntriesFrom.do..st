enumerating
priorEntriesFrom: aReference do: aBlock
	| nextReference |
	nextReference := aReference.
	[ nextReference isNull ] whileFalse: [
		self 
			entryFor: nextReference 
			ifPresent: [ :entry | 
				aBlock value: entry.		
				nextReference := self priorReferenceAt: entry ]
			ifAbsent: [ ^ self ] ]
