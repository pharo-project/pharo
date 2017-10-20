initialization
adapt: aComposablePresenter

	model := aComposablePresenter.
	aComposablePresenter addDependent: self.

	widget := self buildWidget.