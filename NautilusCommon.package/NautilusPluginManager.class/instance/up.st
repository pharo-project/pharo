private
up

	| list selection firstIndex |
	list := pluginsList copy.
	firstIndex := pluginsList size.
	selection := self selectedPlugins.
	selection do: [:index || each newIndex |
		each := list at: index.
		firstIndex := index-1 min: firstIndex.
		newIndex := index-1 max: 1.
		pluginsList removeAt: index.
		pluginsList add: each beforeIndex: newIndex .
		].
	
	self pluginsSelectedIndex: firstIndex.
	
	self changed: #getPluginsList.
	self changed: #allSelections.