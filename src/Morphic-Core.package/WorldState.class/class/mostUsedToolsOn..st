world menu items
mostUsedToolsOn: aBuilder
	<worldMenu>
	(aBuilder group: #MostUsedTools)
		withSeparatorAfter;
		order: 0;
		with: [ (aBuilder item: #'System Browser')
				action: [ Smalltalk tools openClassBrowser ];
				keyText: 'o, b';
				icon: Smalltalk tools browser taskbarIcon.
			(aBuilder item: Smalltalk tools workspace title asSymbol)
				action: [ Smalltalk tools openWorkspace ];
				keyText: 'o, w';
				icon: Smalltalk tools workspace taskbarIcon.
			Smalltalk globals
				at: #TestRunner
				ifPresent: [ :class | 
					(aBuilder item: #'Test Runner')
						action: [ Smalltalk tools openTestRunner ];
						keyText: 'o, u';
						icon: class taskbarIcon ].
			Smalltalk globals
				at: #GTSpotter
				ifPresent: [ :class | 
					(aBuilder item: #Spotter)
						action: [ GTSpotterGlobalShortcut openGlobalSpotter ];
						keyText: 'Shift + Enter';
						iconName: #smallFindIcon ].
			(aBuilder item: #'Monticello Browser')
				action: [ Smalltalk tools openMonticelloBrowser ];
				keyText: 'o, p';
				icon: Smalltalk tools monticelloBrowser taskbarIcon ]