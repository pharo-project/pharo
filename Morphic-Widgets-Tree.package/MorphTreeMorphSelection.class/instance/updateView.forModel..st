view updating
updateView: aTreeMorph forModel: aTreeModel
	aTreeMorph updateFromSelection: self.
	aTreeModel selectionChanged