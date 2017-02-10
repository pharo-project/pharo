tests
fileOut: epiceaEntries

	| originalReference anEvent exportedReference visitor map |	
	map := Dictionary new.
	visitor := EpReferenceFixerVisitor newWithMap: map.
	
	epiceaEntries do: [ :anEntry | 
		originalReference := (anEntry tagAt: OmStore selfReferenceKey).

		anEvent := anEntry content accept: visitor.
		
		exportedReference := outputLog addEntryWith: anEvent tags: [ :tags |  ].
		
		map at: originalReference put: exportedReference.
		].
	
	outputLog store flush.