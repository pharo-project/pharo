private-model
addItem: anItem
	self context streamed add: anItem.
	self context addItem: anItem