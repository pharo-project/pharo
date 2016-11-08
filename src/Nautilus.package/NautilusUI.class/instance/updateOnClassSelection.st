private
updateOnClassSelection
	self changed: #hierarchyButtonEnabled.
	self changed: #browseVariablesButtonEnabled.
	self setWindowTitle.
	self resetCategoriesListSelection.
	self resetMethodsListSelection.
	self updateCategoryAndMethodList.
	self changed: #isAClassSelected.
	self changed: #instanceButtonLabel.
	self commentTextModel setInitialText: self getComments.
