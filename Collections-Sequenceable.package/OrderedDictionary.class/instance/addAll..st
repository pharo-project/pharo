adding
addAll: anAssociationCollection
	"Since Collection implements #associationsDo, this method can accept any collection
	of associations including Arrays and OrderedCollections and not just Dictionaries"

	anAssociationCollection associationsDo: [:each | self add: each].
	^ anAssociationCollection.