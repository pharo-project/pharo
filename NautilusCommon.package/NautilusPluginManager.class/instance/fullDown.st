private
fullDown

	| list selection |
	list := pluginsList copy.
	selection := self selectedPlugins.
	selection doWithIndex: [:idx :count || each |
		each := list at: idx.
		pluginsList removeAt: idx-count+1.
		pluginsList addLast: each].

	self changed: #getPluginsList.
	self changed: #allSelections.