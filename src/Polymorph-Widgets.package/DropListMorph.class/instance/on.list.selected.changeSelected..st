protocol
on: anObject list: getListSel selected: getSelectionSel changeSelected: setSelectionSel
	"Set the receiver to the given model parameterized by the given message selectors."

	getListSel isSymbol
		ifTrue: [self  getListSelector: getListSel]
		ifFalse: [self list: getListSel]. "allow direct list"
	self
		model: anObject;
		getIndexSelector: getSelectionSel;
		setIndexSelector: setSelectionSel;
		updateList;
		updateListSelectionIndex;
		updateContents