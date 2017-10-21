accessing
model: aModel

	super model: aModel.

	aModel model focusOrder
		addLast: toolbar.
	toolbar focusOrder addLast: model