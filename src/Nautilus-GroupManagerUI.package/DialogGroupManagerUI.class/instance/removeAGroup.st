private
removeAGroup
	self groupManager groups removeAGroup: self selectedGroup.
	groupTree updateList.
	groupTreeModel selection: nil.
	groupTreeModel changed: #selection