protocol
removeKey: key 
	"Remove key from the receiver.
	If key is not in the receiver, notify an error."
	
	^ self removeKey: key ifAbsent: [self errorKeyNotFound: key].