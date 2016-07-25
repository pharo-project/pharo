api
updateList
	"<api: #do>"
	"Refresh the list"
	
	self changed: #listElementAt:.
	self resetSelection