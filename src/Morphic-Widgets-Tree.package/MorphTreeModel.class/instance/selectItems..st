selecting
selectItems: aListOfItems
	self selection: nil.
	self requestView: ( MorphTreeChangeRequest selectItems: aListOfItems )