display
newContentMorph

	| row column |
	row := PanelMorph new
				layoutPolicy: TableLayout new;
				listDirection: #leftToRight;
				vResizing: #spaceFill;	
				hResizing: #spaceFill;
				spaceFillWeight: 3;
				addMorph: self buildButtonsColumn;
				addMorph: self buildPluginsBox;
				addMorph: self buildPluginClassesBox.
	column := PanelMorph new
					layoutPolicy: TableLayout new;
					listDirection: #topToBottom;
					vResizing: #spaceFill;	
					hResizing: #spaceFill;
					addMorph: self buildDescriptionTextArea;
					addMorph: row.
	^ column