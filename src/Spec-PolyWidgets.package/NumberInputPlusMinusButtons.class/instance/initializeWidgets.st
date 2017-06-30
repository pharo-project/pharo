initialization
initializeWidgets
	input := self instantiate: TextInputFieldModel .
	plusMinusButtons := self instantiate: PlusMinusButtons .
	self focusOrder add: input; add:plusMinusButtons