api
addItemLabeled: aString do: aBlock icon: anIcon

	| item |
	item := DropListItem named: aString do: aBlock.
	item icon: anIcon.
	"If you add directly in the contents, the update is not triggered from the value holder"
	listHolder add: item.