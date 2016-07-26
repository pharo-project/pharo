settings
settingsOn: aBuilder
	<systemsettings>
	(aBuilder group: #refactoring)
		label: 'Refactoring Engine';
		parent: #codeBrowsing;
		description: 'Settings related to the refactoring tools';
		with: [ 
					(aBuilder setting: #undoSize)
						target: self;
						label: 'Undo size';
						parentName: #refactoring ]