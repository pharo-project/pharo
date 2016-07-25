private
next: numberOfDesiredItems from: initialEntryReference
	"Answer up to a numberOfDesiredItems starting at initialEntryReference."

	| result nextEntryReference |
	result := OrderedCollection new.
	nextEntryReference := initialEntryReference.
	
	[ nextEntryReference isNull ] whileFalse: [
		| item |
		item := result size >= numberOfDesiredItems
			ifTrue: [
				self newFetchMoreItemFor: nextEntryReference in: rootItems ]
			ifFalse: [
				self itemFor: nextEntryReference ].
		
		result addLast: (
			result 
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