api
addItemLabeled: aString do: aBlock

	| item |
	item := DropListItem named: aString do: aBlock.
	
	"If you add directly in the contents, the update is not triggered from the value holder"
	listHolder add: item.