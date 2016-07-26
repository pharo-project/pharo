display
buildPluginsBox

	^ GroupboxMorph new
			addContentMorph: self buildPluginsList;
			addContentMorph: self buildRemoveButton;
			label: 'Selected plugins:';
			hResizing: #spaceFill;
			vResizing: #spaceFill.