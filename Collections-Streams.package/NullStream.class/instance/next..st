reading
next: anInteger 
	"Answer the next anInteger elements of my collection. Must override 
	because default uses self contents species, which might involve a large 
	collection."

	position := position + anInteger.
	^self collectionSpecies new: anInteger