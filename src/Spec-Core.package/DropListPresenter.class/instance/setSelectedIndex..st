api
setSelectedIndex: anIndex
	"<api: #integer min:1 max:#listSize getter: #selectedIndex>"
	"Force the selection of the item at index anIndex"
	| selection |

	selection := listHolder value at: anIndex ifAbsent: [ ^ self ].
	selection value.
	selectionHolder index value: anIndex.
	selectionHolder selection value: selection