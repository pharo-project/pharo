morphic ui
workingCopyTreeMorph
	^ SimpleHierarchicalListMorph
		on: self
		list: #workingCopyTree
		selected: #workingCopyWrapper
		changeSelected: #workingCopyWrapper:
		menu: #workingCopyListMenu: