initialization
initializeWidgets
	text := self instantiate: TextInputFieldModel.
	text
		text: 'Object is nil';
		enabled: false.