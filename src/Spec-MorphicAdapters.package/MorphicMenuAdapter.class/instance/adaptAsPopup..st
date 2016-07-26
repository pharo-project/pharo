initialization
adaptAsPopup: aComposableModel

	model := aComposableModel.
	aComposableModel addDependent: self.
	widget := self buildWidgetPopup.