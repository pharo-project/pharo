selecting
methodSelection: aNumber
	methodSelection := aNumber = 0 ifFalse: [self visibleMethods at: aNumber].
	self changed: #methodSelection; changed: #text.