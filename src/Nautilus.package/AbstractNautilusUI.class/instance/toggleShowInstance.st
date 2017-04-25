buttons behavior
toggleShowInstance

	self showInstance: self showInstance not.
	self changed: #instanceButtonState.
	self categoryWidget label: self categoriesLabel.
	self methodWidget label: self methodsLabel.
	instanceButtonWidget helpText: (self instanceButtonHelpTextFor: self instanceButtonState).