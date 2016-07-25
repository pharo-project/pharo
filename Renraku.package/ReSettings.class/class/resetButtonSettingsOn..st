settings-definition
resetButtonSettingsOn: aBuilder
	<systemsettings>
		
	(aBuilder group: #'resetRules')
		order: 10;
		target: self;
		label: 'Rule cache';
		parent: #renraku;
		description: 'Renraku (the engine behind quality rules) caches the rule objects for performance boost. Here you can reset the cache, this may be useful if you''ve implemented a new rule and want to include it.';
		dialog: [
			SimpleButtonMorph new
				target: ReRuleManager;
				label: 'Reset Renraku rule cache';
				actionSelector: #resetWithInform;
				themeChanged;
				yourself ].