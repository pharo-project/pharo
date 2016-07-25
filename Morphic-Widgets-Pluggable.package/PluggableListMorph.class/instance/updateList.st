updating
updateList
	| index |
	"the list has changed -- update from the model"
	self listMorph listChanged.
	self setScrollDeltas.
	scrollBar setValue: 0.0.
	index := self getCurrentSelectionIndex.
	self resetPotentialDropRow.
	index ifNotNil: [ 
		index := index min: self getListSize.
		index > 0 ifTrue: [	self selectionIndex: index].
	].
	
	self searchedElement: nil.
