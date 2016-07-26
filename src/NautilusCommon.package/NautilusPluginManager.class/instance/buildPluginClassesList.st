items creation
buildPluginClassesList

	| list |
	list := PluggableListMorph
		on: self
		list: #getPluginClassesList
		primarySelection: #pluginClassesSelectedIndex
		changePrimarySelection: #pluginClassesSelectedIndex:
		listSelection: #pluginClassesSelectedAt:
		changeListSelection: #pluginClassesSelectedAt:put:
		menu: nil
		keystroke: nil.
		
	list wrapSelector: #pluginName.
		
	list resetListSelector: #resetPluginClassesListSelection.
	list autoDeselect: true.
	
	list
		vResizing: #spaceFill;
		hResizing: #spaceFill.
	^ list