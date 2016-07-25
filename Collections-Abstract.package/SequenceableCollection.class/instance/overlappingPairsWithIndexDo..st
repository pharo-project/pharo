enumerating
overlappingPairsWithIndexDo: aBlock 
	"Emit overlapping pairs of my elements into aBlock, along with an index."

	1 to: self size - 1
		do: [:i | aBlock value: (self at: i) value: (self at: i + 1) value: i ]