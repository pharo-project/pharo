accessing
at: key ifAbsentPut: aBlock
	"Return the value at the given key.
	If the key is not included in the receiver store and return the result 
	of evaluating aBlock as the new value."
	
	| index |
	index := self findIndexForKey:  key.
	^ index = 0 
		ifFalse: [values at: index]
		ifTrue: [self privateAt: key put: aBlock value]