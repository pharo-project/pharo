private
down

	| list selection firstIndex maxSize |
	list := pluginsList copy.
	maxSize := pluginsList size.
	firstIndex := maxSize.
	selection := self selectedPlugins.
	selection reverse do: [:index || each newIndex |
		each := list at: index.
		firstIndex := index+1 min: firstIndex.
		newIndex := index+1 min: maxSize.
		pluginsList removeAt: index.
		pluginsList add: each beforeIndex: newIndex .
		"set selection"
		].
	self pluginsSelectedIndex: firstIndex.
	
	self changed: #getPluginsList.
	self changed: #allSelections.