buttons behavior
removeButtonAction
	
	self selectedPlugins reverse do: [:item | self pluginsList remove: item ].
	self resetPluginsListSelection.
	tree deselectAll.
	tree model setSelection: nil.
	tree update: #rootNodes