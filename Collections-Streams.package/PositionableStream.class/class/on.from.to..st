instance creation
on: aCollection from: firstIndex to: lastIndex 
	"Answer an instance of me, streaming over the elements of aCollection 
	starting with the element at firstIndex and ending with the one at 
	lastIndex."

	^self basicNew on: (aCollection copyFrom: firstIndex to: lastIndex)