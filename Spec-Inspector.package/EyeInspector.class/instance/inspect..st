inspecting
inspect: anObject 
	"This is called to update the inspector model with a new object, updates the UI"

	self object: anObject.
	self list setSelectedIndex: 1.
	self refresh
	