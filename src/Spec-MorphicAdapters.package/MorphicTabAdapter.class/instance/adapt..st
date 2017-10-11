initialization
adapt: aComposablePresenter
	widget := self buildWidgetWith: aComposablePresenter.
	aComposablePresenter addDependent: widget.
	self bindModel: aComposablePresenter andWidget: widget