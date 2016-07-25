menu builder
sourceCodeMenu: aMenu shifted: shifted
	| menu |
	menu := (PragmaMenuBuilder
		pragmaKeyword: self sourceTextWidget editingModeClass menuKeyword
		model: self sourceTextWidget) menu.
	^ aMenu
		addAllFrom: self sourceCodeMenuBuilder menu;
		addLine;
		addAllFrom: menu;
		yourself