settings
defaultDiffToolsSettingOn: aBuilder
	<systemsettings>  
	(aBuilder setting: #polymorphDiffToolsUsedByDefault)
		label: 'Use Polymorph difference tools';
		description: 'When enabled the Polymorph diff tools will be used with Monticello. When disabled, the original tools are used.';
		parent: #differator;
		target: PSMCPatchMorph;
		selector: #usedByDefault