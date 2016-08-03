private
next: numberOfDesiredItems from: initialEntryReference
	"Answer up to a numberOfDesiredItems starting at initialEntryReference."

	| result nextEntryReference |
	result := OrderedCollection new.
	nextEntryReference := initialEntryReference.
	
	[ nextEntryReference isNull ] whileFalse: [
		| item |
		result size >= numberOfDesiredItems ifTrue: [
			result add: (self newFetchMoreItemFor: nextEntryReference in: rootItems).
			^ result ]. "Enough items"

		item := self log
			entryFor: nextEntryReference
			ifPresent: [ :anEntry | 
				self 
					filter: anEntry 
					ifAccepted: [ self newItemFor: anEntry ]
					ifNotAccepted: [ self newFilteredItemFor: anEntry ] ]
			ifAbsent: [ ^ result ]. "Not found then can't continue"

		"Combine item if possible"
		result addLast: 
			(result 
				ifEmpty: [ item ]
				ifNotEmpty: [
					result last 
						isCombinableWith: item
						ifTrue: [ :combinedItem |
							result removeLast.
							combinedItem ]
						ifFalse: [ item ] ]).
		
		nextEntryReference := item priorEntryReference ].

	^ result