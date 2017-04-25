private
next: numberOfDesiredItems from: initialEntryReference
	"Answer up to a numberOfDesiredItems starting at initialEntryReference."

	| result nextEntryReference lastGlobalName |
	result := OrderedCollection new.
	initialEntryReference isNull ifTrue: [ ^ result ].

	nextEntryReference := initialEntryReference.
	lastGlobalName := initialEntryReference globalName.
	
	[	| item |
		result size >= numberOfDesiredItems ifTrue: [
			result add: (self newFetchMoreItemFor: nextEntryReference in: rootItems).
			^ result ]. "Enough items"

		item := [
			self
				newItemFor: nextEntryReference
				ifAbsent: [ ^ result ] "Entry should belong to other log => stop here"
			] 	on: OmFileStoreReadingError 
				do: [
					"Reading error => stop here"
					self inform: 'Epicea: Reading error at ', nextEntryReference asString.
					^ result ].

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

		nextEntryReference globalName = lastGlobalName ifFalse: [
			result add: (EpLogHeaderItem new entryReference: nextEntryReference; yourself).
			lastGlobalName := nextEntryReference globalName ].
		
		nextEntryReference := item priorEntryReference.

	] doWhileFalse: [ nextEntryReference isNull ].

	^ result