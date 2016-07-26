settings
catalogSettingsOn: aBuilder
	<systemsettings>
	(aBuilder group: #gtspotter)
		parent: #gt;
		label: 'GT Spotter extensions';
		description: 'All extensions of GT Spotter';
		with:
				[ GTSpotter spotterExtendingMethods
				do:
					[ :each | 
					(aBuilder setting: (self internalKeyForMethod: each))
						target: self;
						label: each displayString;
						description: each sourceCode;
						default: true] ]