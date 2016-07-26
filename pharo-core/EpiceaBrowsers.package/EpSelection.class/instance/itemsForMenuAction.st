accessing
itemsForMenuAction
	"Answer a sequenced collection of items as for applying a menu action."

	^ self items reject: [ :item | item ignoreForMenuAction ]