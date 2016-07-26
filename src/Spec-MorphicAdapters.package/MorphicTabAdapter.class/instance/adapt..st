initialization
adapt: aComposableModel
	widget := self buildWidgetWith: aComposableModel.
	aComposableModel addDependent: widget.
	self bindModel: aComposableModel andWidget: widget