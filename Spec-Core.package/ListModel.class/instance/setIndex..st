private
setIndex: anIndex
	"Set the index of the selected item when you click on an item"

	self allowToSelect ifFalse: [ ^ self ].
	self okToChange ifFalse: [ ^ self ].
	selectionHolder index value: anIndex.
	selectionHolder selection value: (self listElementAt: anIndex).