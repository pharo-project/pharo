settings
differatorSettingsOn: aBuilder 
	<systemsettings>
	(aBuilder group: #differator) 
		label: 'Code Diffs';
		parent: #codeBrowsing; 
		description: 'All settings concerned with the computation of diffs' ;
		with: [	
			(aBuilder setting: #ignoreLineEndings) 
				label: 'Ignore line endings';
				target: self;
		 		description: 'When selected, line ending differences will be ignored ' ];
		with: [  
		(aBuilder setting: #diffsWithPrettyPrint)
				target: self;	
				label: 'Pretty print differences' ;
				description: 'If true, displays of source code differences will be pretty-printed first' 
		]