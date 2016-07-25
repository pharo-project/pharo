private
silentlySetSelectedIndex: anIndex
	"Force the selection of the item at index anIndex"
	| selection |

	selection := listHolder value at: anIndex ifAbsent: [ ^ self ].
	selectionHolder index value: anIndex.
	selectionHolder selection value: selection.