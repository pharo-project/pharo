private
fullUp

	| list selection |
	list := pluginsList copy.
	selection := self selectedPlugins.
	selection reverse doWithIndex: [:idx :count || each |
		each := list at: idx.
		pluginsList removeAt: (idx+count-1).
		pluginsList addFirst: each].
	
	self changed: #getPluginsList.
	self changed: #allSelections.