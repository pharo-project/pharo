Extension { #name : 'NonInteractiveErrorHandler' }

{ #category : '*System-Settings-Core' }
NonInteractiveErrorHandler class >> uiSettingsOn: aBuilder [

	<systemsettings>
	(aBuilder group: #nonInteractive)
		label: 'Headless mode';
		parentName: #pharoSystem;
		description: 'Settings related to mode without UI';
		with: [
				(aBuilder setting: #snapshotErrorImage)
					label: 'Snapshot before quitting';
					defaultValue: false;
					target: self;
					description: 'On unhandled exception, save a new version of the image before quitting' ]
]
