requirements
collectionWithElement
	"Returns a collection that already includes what is returned by #element."
	^ SortedCollection new add: self element ; add: 5 ; add: 2; yourself.