api
items: aList
	"Populate the drop list with a list of ui specific items"
	"aList is a list of domain specific objects. If you want to specify more precisely the item actions, see #addItemLabeled:do:"
	"<api: #st getter: #listItems>"
	
	| dropListItems |
	dropListItems := aList collect: [ :e | DropListItem on: e do: [] ].
	listHolder value: dropListItems