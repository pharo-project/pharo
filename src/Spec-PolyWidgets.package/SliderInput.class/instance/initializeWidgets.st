initialization
initializeWidgets
	slider := self instantiate: SliderModel.
	input := self instantiate: TextInputFieldModel.
	self focusOrder add: input