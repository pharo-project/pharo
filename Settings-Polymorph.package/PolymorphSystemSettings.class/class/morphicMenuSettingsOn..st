morphic
morphicMenuSettingsOn: aBuilder 
	(aBuilder group: #menu)
		label: 'Menus';
		parent: #morphic;
		description: 'All menu settings' ;
		target: UITheme;
		targetSelector: #currentSettings;
		noOrdering;
		with: [
			(aBuilder setting: #preferGradientFill)
				label: 'Gradient filling' ;
				description: 'If true, then menus will have a gradient look' .
			(aBuilder setting: #flatMenu)
				label: 'Flat appearance' ;
				description: 'If true, then menu will without 3D like borders.' .
			(aBuilder setting: #autoMenuColor)
				label: 'Computed color' ;
				description: 'If true, then the menu color will be computed from world dressing.' .
			(aBuilder setting: #menuColor)
				label: 'Menu color';
				description: 'The menu color to use if it is not computed automatically (see the ''Computed color'' setting)' .
			(aBuilder setting: #menuSelectionColor)
				label: 'Menu selection color' ;
				description: 'The background color of a selected menu item' .
			(aBuilder setting: #menuSelectionTextColor)
				description: 'The text color of a selected menu item' ;
				label: 'Menu selection text color' .
		]