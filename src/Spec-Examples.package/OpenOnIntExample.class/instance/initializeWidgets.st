initialization
initializeWidgets
	text := self instantiate: LabelPresenter.
	plus := self instantiate: ButtonPresenter.
	minus := self instantiate: ButtonPresenter.
	plus
		label: '+'.
	minus
		label: '-'