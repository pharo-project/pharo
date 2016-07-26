updating
expandChangeTree
	self flag: #TODO. "We should only expand the nodes wich have partial changes, if a class was added or removed there is no need to show the complete tree expanded"
	self changeTree expandAll.