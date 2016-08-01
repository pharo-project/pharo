initialization
initializeWidgets
	text := self instantiate: LabelModel.
	plus := self instantiate: ButtonModel.
	minus := self instantiate: ButtonModel.
	plus
		label: '+'.
	minus
		label: '-'