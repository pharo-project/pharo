enumerating
priorEntriesFrom: initialReference upTo: finalReference

	| result |
	result := OrderedCollection new.
	
	self 
		priorEntriesFrom: initialReference 
		do: [ :anEntry |
			result add: anEntry.
			(self referenceTo: anEntry) = finalReference ifTrue: [ ^ result ].			
		].
	
	^ result