accessing
next: anInteger 
	"Answer the next anInteger elements of my collection. Must override 
	because default uses self contents species, which might involve a large 
	collection."

	| newArray |
	newArray := collection species new: anInteger.
	1 to: anInteger do: [:index | newArray at: index put: self next].
	^newArray