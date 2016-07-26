accessing
selectedCategoryInternally: anObject
	self resetMethodsListSelection.
	anObject ifNotNil: [ self giveFocusTo: categoryWidget ]