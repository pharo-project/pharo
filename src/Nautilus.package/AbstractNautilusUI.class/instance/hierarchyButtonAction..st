buttons behavior
hierarchyButtonAction: aBoolean

	self hierarchyButtonEnabled
		ifTrue: [ 
			self toggleShowHierarchy.
			self changed: #hierarchyButtonEnabled. ]