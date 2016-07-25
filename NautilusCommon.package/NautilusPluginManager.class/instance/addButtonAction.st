buttons behavior
addButtonAction
	
	self selectedPluginClasses do: [:each | 
		self pluginsList addLast: { each. each defaultPosition } ].
	tree update: #rootNodes