Extension { #name : 'OupsDebuggerSelector' }

{ #category : '*System-Settings-Core' }
OupsDebuggerSelector class >> handleDebuggerErrorsSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #handleDebuggerErrors)
		label: 'Handle debugger errors';
		target: self;
		default: false;
		parent: #debugging;
		description: 'Try to debug debugger errors when they occur.'
]
