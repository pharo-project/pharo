instance creation
on: aCollection from: firstIndex to: lastIndex 
	"Answer an instance of me on a copy of the argument, aCollection, 
	determined by the indices firstIndex and lastIndex. Position the instance 
	at the beginning of the collection."

	^self basicNew
		on: aCollection
		from: firstIndex
		to: lastIndex