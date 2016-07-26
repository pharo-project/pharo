writing
nextPutAll: aCollection
	"Append the elements of aCollection to the sequence of objects accessible 
	by the receiver. Answer aCollection."

	position := position + aCollection size.
	aCollection isEmpty
		ifFalse: [ lastElement := aCollection last ].
	^ aCollection