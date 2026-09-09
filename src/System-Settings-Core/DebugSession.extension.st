Extension { #name : 'DebugSession' }

{ #category : '*System-Settings-Core' }
DebugSession class >> fastStepThroughModeSettingOn: aBuilder [

	<systemsettings>
	(aBuilder setting: #fastStepThroughMode)
		label: 'Enable the fast step through mode';
		target: self;
		parent: #debugging;
		default: false;
		description:
			'If enabled, the debugger will perform step through operations using halting blocks. This implementation is more efficient than the original one based on interpreter steps'
]
