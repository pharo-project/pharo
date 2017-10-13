initialization
initializeWidgets
	input := self instantiate: TextInputFieldPresenter .
	plusMinusButtons := self instantiate: PlusMinusButtons .
	self focusOrder add: input; add:plusMinusButtons