api
selectDialog: anObject
	"<api: #block getter: #selectDialog registration: #whenSelectDialogChanged:>"
	"Set a block that returns an object that will 
	first be sent #openDialogWithSpec
	and then as okAction sent #selectedItem"
	selectDialog value: anObject