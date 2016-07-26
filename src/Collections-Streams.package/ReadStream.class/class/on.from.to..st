instance creation
on: aCollection from: firstIndex to: lastIndex 
	"Answer with a new instance streaming over a copy of aCollection from
	firstIndex to lastIndex."

	^self basicNew
		on: aCollection
		from: firstIndex
		to: lastIndex