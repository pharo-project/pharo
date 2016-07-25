converting
asSortedCollection: aCollection
	"Create a sorted collection from the given input"
	^aCollection asSortedCollection: [ :tA :tB | tA tally >= tB tally ]