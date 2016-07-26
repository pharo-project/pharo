putting
at: key put: value 
	"Set the value at key to be anObject.  If key is not found, create a
	new entry for key and set is value to anObject. Answer anObject."

	| index |
	index := self findIndexForKey:  key.
	^ index = 0
		ifFalse: [values at: index put: value]
		ifTrue: [self privateAt: key put: value]	
	