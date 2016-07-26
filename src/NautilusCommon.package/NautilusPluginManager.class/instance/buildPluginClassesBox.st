display
buildPluginClassesBox

	^ GroupboxMorph new
			addContentMorph: self buildPluginClassesList;
			addContentMorph: self buildAddButton;
			label: 'Available plugin classes:';
			hResizing: #spaceFill;
			vResizing: #spaceFill.