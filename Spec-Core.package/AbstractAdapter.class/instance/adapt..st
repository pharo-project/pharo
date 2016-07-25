initialization
adapt: aComposableModel

	model := aComposableModel.
	aComposableModel addDependent: self.

	widget := self buildWidget.