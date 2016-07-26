read, write, position
nextPutAll: aCollection 
	"Append the elements of aCollection to the sequence of objects accessible 
	by the receiver. Answer aCollection."

	^ writer nextPutAll: aCollection