enumerating
overlappingPairsDo: aBlock 
	"Emit overlapping pairs of my elements into aBlock"

	1 to: self size - 1
		do: [:i | aBlock value: (self at: i) value: (self at: i + 1)]