Extension { #name : 'OupsDebuggerSelector' }

{ #category : '*System-Settings-Core' }
OupsDebuggerSelector class >> handleDebuggerErrorsSettingsOn: aBuilder [

	<systemsettings>
	(aBuilder setting: #handleDebuggerErrors)
		label: 'Handle debugger errors';
		target: self;
		defaultValue: false;
		parentName: #debugging;
		description: 'Try to debug debugger errors when they occur.'
]
