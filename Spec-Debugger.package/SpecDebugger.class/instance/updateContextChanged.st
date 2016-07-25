updating actions
updateContextChanged
	|selection|
	
	selection := self stack selectedItem.
	self updateStep.
	self stack setSelectedItem: selection