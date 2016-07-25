to clean later
at: aKey put: anObject 
	"Set the global at key to be anObject.  If key is not found, create a
	new entry for key and set is value to anObject. Answer anObject."

	^globals at: aKey put: anObject