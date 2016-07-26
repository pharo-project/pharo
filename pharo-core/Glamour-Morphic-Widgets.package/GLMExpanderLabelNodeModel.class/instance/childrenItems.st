callbacks
childrenItems
	^ OrderedCollection 
		with: ((GLMExpanderContentsNodeModel with: (self->self item)) containerNode: self)