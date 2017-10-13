api
selectedItems: aCollection
	"<api: #st getter: #selectedItems registration: #whenSelectedItemsChanged:>"
	"Set the selected item"
	"In the case of a multiple selection list, it returns the last selected item"

	"treePresenter selectNodePath: aCollection."
	selectionHolder value: aCollection.
	