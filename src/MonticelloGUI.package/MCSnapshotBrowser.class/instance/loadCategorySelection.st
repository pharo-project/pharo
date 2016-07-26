menus
loadCategorySelection
	"Load the entire selected category"
	categorySelection ifNil: [ ^self ].
	self methodsForSelectedClassCategory do: [ :m | m load ].