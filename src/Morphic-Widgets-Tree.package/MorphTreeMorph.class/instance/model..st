accessing
model: aTreeModel
	self model ifNotNil: [self model announcer unsubscribe: self].
	super model: aTreeModel.
	self registerRequestHandlers
