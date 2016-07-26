initialize
initialize

	super initialize.

	treeModel := SpecTreeModel model: self.	
	treeModel announcer
		when: MorphTreeSelectionChanged
		send: #onMorphTreeSelectionChanged:
		to: self.