settings
displaySettingsOn: aBuilder 
	<systemsettings>
	(aBuilder setting: #displayFullscreen)
		label: 'Fullscreen mode';
		parent: #desktopSettings;
		target: #Display;
		getSelector: #isFullscreen;
		setSelector: #fullscreen:;
		description: 'On platforms that support it, set fullscreen mode';
		default: false.
