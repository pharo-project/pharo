initialization
initializePresenter
	super initializePresenter.
	self flag: 'some of this logic could be moved to the stack widget'.
	self flag: 'The toolbar should not be updated when the list changes, but when an action is perormed.'.
	self stack whenListChanged: [ :aList |
		aList isEmpty ifFalse: [ self stack setSelectedItem: aList first ].
		"Updating the toolbar will result in changing the button widgets.
		If the widget hasn't been opened, there will be no spec, which leads to an error."
		self spec ifNotNil: [
			self updateToolbar ] ].

	self stack whenSelectedItemChanged: [:aContext |
		self updateCodeFromContext: aContext.
		self updateInspectorsFromContext: aContext.
		self stack updateForSelectionChanged ].