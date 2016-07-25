private
setSelectionStateFor: anIndex at: aBoolean
	"Set the selection state of the item at index _anIndex_ in the case of multiple selection list"

	self allowToSelect ifFalse: [ ^ self ].
	^ multiSelectionHolder at: anIndex put: aBoolean