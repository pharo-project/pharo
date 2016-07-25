special objects
specialSelectorAt: anInteger 
	"Answer the special message selector from the interleaved specialSelectors array."

	^ (self specialObjectsArray at: 24) at: anInteger * 2 - 1