settings
debugSettingsOn: aBuilder
	<systemsettings>
	
	(aBuilder setting: #enableStackColoring)
		label: 'Color the stack' translated;
		target: GTGenericStackDebugger;
		parent: #debugging;
		description:
				'When true, the stack context are colored based on their relationship with the selected context.' translated.
	
	(aBuilder setting: #enableDebuggerWindowDistinctColor)
		label: 'Color the window' translated;
		target: GTGenericStackDebugger;
		parent: #debugging;
		description:
				'When true, the window border is colored with red to make it distinct from other windows.' translated.
				
	(aBuilder setting: #GTDebuggerStatus)
		parent: #gt;
		target: GTGenericStackDebugger;
		getSelector: #isGTDebuggerEnabled;
		setSelector: #setGTDebuggerEnabledStatus:; 
		label: 'GTDebugger';
		description: 
			'Enable or disable the GTDebugger.'
	
