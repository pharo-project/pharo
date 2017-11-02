Extension { #name : #WriteStream }

{ #category : #'*Fuel' }
WriteStream >> nextBytesPutAll: aCollection [
	"Append the bytes of aCollection to the sequence of bytes accessible 
	by the receiver. Answer aCollection."
 
 	| newEnd |
 	collection class instSpec == aCollection class instSpec ifFalse:
 		[^ super nextPutAll: aCollection ].
 
 	newEnd := position + aCollection size.
 	newEnd > writeLimit ifTrue:
 		[self growTo: newEnd + 10].
 
 	collection replaceFrom: position+1 to: newEnd  with: aCollection startingAt: 1.
 	position := newEnd.
	
	^ aCollection
]
