accessing
list: anObject
	"Set the value of list"

	list := anObject.
	self
		changed: #list;
		changed: #selectionIndex