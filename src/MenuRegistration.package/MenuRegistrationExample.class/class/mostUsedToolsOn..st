menu declaring
mostUsedToolsOn: aBuilder 
	<worldMenuExample>
	(aBuilder group: #MostUsedTools)
		withSeparatorAfter;
		order: 0;
		target: Smalltalk tools;
		with: [
			(aBuilder item: #'System browser') selector: #openClassBrowser; icon: Smalltalk tools browser taskbarIcon.
			(aBuilder item: #Workspace) selector: #openWorkspace; icon: Smalltalk tools workspace taskbarIcon.
			]