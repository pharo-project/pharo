settings
shouldSwapMenuOpenerButtonsSetting: aBuilder
	<systemsettings>
	(aBuilder setting: #shouldSwapMenuOpenerButtons)
		label: 'Invert right and left click on world menu in Linux and Windows';
		parent: #desktopSettings;
		target: self;
		getSelector: #shouldSwapMenuOpenerButtons;
		setSelector: #shouldSwapMenuOpenerButtons:;
		description:
				'If checked the behaviour of the world menu is changed only under linuex and window. The main menu will appear on right click instead of left click.';
		default: false