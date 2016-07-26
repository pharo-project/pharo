initialization
on: anObject selected: getSelectionSel changeSelected: setSelectionSel
	"Set the receiver to the given model parameterized by the given message selectors."

	self
		model: anObject;
		getStateSelector: getSelectionSel;
		setStateSelector: setSelectionSel;
		updateSelection