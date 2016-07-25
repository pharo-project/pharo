api
allowToSelect: aBoolean
	"<api: #boolean getter: #allowToSelect registration: #whenAllowToSelectChanged:>"
	"Set if the list items can be selected or not"

	allowToSelect value: aBoolean.
	aBoolean 
		ifFalse: [ self resetSelection ].