user interface
morphicDirectoryTreePaneFiltered: aSymbol
	^(SimpleHierarchicalListMorph 
		on: self
		list: aSymbol
		selected: #selectedDirectory
		changeSelected: #selectedDirectory:
		menu: #volumeMenu:
		keystroke: nil)
			autoDeselect: false;
			enableDrag: false;
			enableDrop: true;
			yourself
		
