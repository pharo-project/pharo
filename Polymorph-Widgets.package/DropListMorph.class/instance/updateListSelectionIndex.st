updating
updateListSelectionIndex
	"Update the list selection."

	|i|
	self useSelectionIndex
		ifTrue: [i := self getCurrentSelectionIndex.
				listSelectionIndex = i ifTrue: [^self].
				listSelectionIndex := i]
		ifFalse: [i := self getCurrentSelection.
				listSelectionIndex := i isNil
					ifTrue: [0]
					ifFalse: [self list indexOf: i]].
	self
		changed: #listSelectionIndex;
		updateContents;
		triggerEvent: #selectionIndex with: i