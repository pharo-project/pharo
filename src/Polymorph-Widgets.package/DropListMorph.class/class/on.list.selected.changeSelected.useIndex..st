instance creation
on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel useIndex: useIndex
	"Answer a new instance of the receiver on the given model using
	the given selectors as the interface."

	^self new
		useSelectionIndex: useIndex;
		on: anObject 
		list: getListSel
		selected: getSelectionSel
		changeSelected: setSelectionSel