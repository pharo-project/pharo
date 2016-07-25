writing
newEntry: anEntry

	| newReference localName |
	"Before writing, tag the entry with an identifier"
	localName := (entryCount + 1) asString.
	newReference := 
		OmReference 
			globalName: globalName
			localName: localName.
	anEntry tags 
		at: self selfReferenceKey 
		put: newReference.

	"Write, and update only if success."		
	self 
		writeEntry: anEntry 
		ifSuccess: [ :position |
			"cache entry"
			entryPositionsByLocalName
				at: localName
				put: position.

			"cache entry count"
			entryCount := entryCount + 1.
			
			"update head"
			headReference := newReference. 
			].