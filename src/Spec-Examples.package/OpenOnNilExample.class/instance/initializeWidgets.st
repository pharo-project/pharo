initialization
initializeWidgets
	text := self instantiate: TextInputFieldPresenter.
	text
		text: 'Object is nil';
		enabled: false.