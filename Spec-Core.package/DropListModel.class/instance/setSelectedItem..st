api
setSelectedItem: anItem
	"Force the selection of the item anItem"
	"<api: #st getter: #selectedItem>"
	| index realItem |
	index := self listItems indexOf: anItem ifAbsent: [ ^ self ].
	realItem := listHolder at: index.
	selectionHolder index value: index.
	selectionHolder selection value: realItem.