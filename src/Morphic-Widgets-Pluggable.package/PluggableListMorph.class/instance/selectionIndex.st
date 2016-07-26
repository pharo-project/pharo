selection
selectionIndex
	"return the index we have currently selected, or 0 if none"
	^self listMorph selectedRow ifNil: [ 0 ]