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
	self selectedClass
		ifNil: [ ^ self ]
		ifNotNil: [ :class | 
			class hasComment
				ifTrue: [ self classCommentTextModel setInitialText: class comment ]
				ifFalse: [ self classCommentTextModel setInitialText: class classCommentBlank ] ]