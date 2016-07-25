settings
uiSettingsOn: aBuilder
	<systemsettings>
	(aBuilder group: #nonInteractive)
		label: 'Headless mode';
		parent: #pharoSystem;
		description: 'Settings related to mode without UI';
		with: [
			(aBuilder setting: #snapshotErrorImage)
				label: 'Snapshot before quitting' translated;
				target: CommandLineUIManager;
				description: 'On unhandled exception, save a new version of the image before quitting' translated]