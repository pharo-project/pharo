settings
settingsOn: aBuilder 
	<systemsettings>
	
	(aBuilder group: #gt)
		parent: #tools;
		label: 'Glamorous Toolkit';
		with: [
			(aBuilder setting: #numberOfInspectorPanes)
				target: self; 
				label: 'Number of inspector panes';
		 		description: 'Specifies the maximum number of panes that are visible at every moment in a Glamorous Inspector.'.
			(aBuilder setting: #GTInspectorStatus)
				target: self;
				getSelector: #isGTInspectorEnabled;
				setSelector: #setGTInspectorEnabledStatus:; 
				label: 'GTInspector';
		 		description: 'Enable or disable the GTInspector.'.
			(aBuilder setting: #EnabledStepRefreshStatus)
				target: self;
				getSelector: #isStepRefreshEnabled;
				setSelector: #setEnabledStepRefreshStatus:; 
				label: 'GTInspector Step Refresh';
		 		description: 'Enable or disable the automatic refresh in GTInspector that uses a busy update loop (based on the stepping mechanism from Morphic).']