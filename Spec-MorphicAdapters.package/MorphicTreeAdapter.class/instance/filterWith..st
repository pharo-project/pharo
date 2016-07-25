spec protocol
filterWith: aFilter
	
	self widgetDo: [ :w || nodes |
		nodes := w model rootNodes.
		nodes do: [:r | r nodeModel updateAccordingTo: aFilter].
	
		self removeRootsSuchAs: [:n | (aFilter keepTreeNode: n) not and: [n isEmpty]].

		self changed: #rootNodes ].