initialization
adaptAsPopup: aComposablePresenter

	model := aComposablePresenter.
	aComposablePresenter addDependent: self.
	widget := self buildWidgetPopup.