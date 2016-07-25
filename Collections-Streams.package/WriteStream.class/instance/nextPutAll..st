accessing
nextPutAll: aCollection
 
 	| newEnd |
 	collection class == aCollection class ifFalse:
 		[^ super nextPutAll: aCollection ].
 
 	newEnd := position + aCollection size.
 	newEnd > writeLimit ifTrue:
 		[self growTo: newEnd + 10].
 
 	collection replaceFrom: position+1 to: newEnd  with: aCollection startingAt: 1.
 	position := newEnd.
	
	^ aCollection