instance creation
sizeFor: nElements
	"Large enough size to hold nElements with some slop (see fullCheck)"
	
	nElements < 4 ifTrue: [ ^5 ].
	^ HashTableSizes atLeast: nElements +1 * 4 // 3