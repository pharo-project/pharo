api
selectedItem: anItem
	"< api: #st getter: #selectedItem registration: #whenSelectedItemsChanged:>"
	"Set the selected item"
	"In the case of a multiple selection list, it returns the last selected item"
	
	self selectedItems: { anItem }