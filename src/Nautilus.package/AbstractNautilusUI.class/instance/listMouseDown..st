events handling
listMouseDown: anEvent

	self showHierarchy
		ifTrue: [ 
			self showHierarchy: false.
			self packageWidget mouseDown: anEvent ].
