api
items: aList
	"Set the items of the list.
	aList is a collection of your domain specific items.
	Use a two stages notification (issue 7420)."

	"<api: #st getter: #listItems registration: #whenListChanged:>"
	| oldContents |
	oldContents := listHolder value.
	listHolder value: (aList sorted: sortingBlockHolder value).
	listAnnouncer announce: (ValueChanged oldValue: oldContents newValue: listHolder value)