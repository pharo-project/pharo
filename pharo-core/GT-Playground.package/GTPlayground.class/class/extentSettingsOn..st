settings
extentSettingsOn: aBuilder 
	<systemsettings>
	
	(aBuilder setting: #GTPlaygroundPreferredWidth)
		target: self;
		parent: #gt;
		getSelector: #preferredWidth;
		setSelector: #preferredWidth:; 
		label: 'Playground width';
		description: 'The preferred width of new Playground windows'.
	(aBuilder setting: #GTPlaygroundPreferredHeight)
		target: self;
		parent: #gt;
		getSelector: #preferredHeight;
		setSelector: #preferredHeight:; 
		label: 'Playground height';
		description: 'The preferred height of new Playground windows'.
	(aBuilder setting: #GTPlaygroundRememberPreferredExtent)
		target: self;
		parent: #gt;
		getSelector: #rememberPreferredExtent;
		setSelector: #rememberPreferredExtent:; 
		label: 'Remember extent of Playground';
		description: 'Enable or disable remembering of the extent of Playground windows on resize. When the preference is true, resizing a Playground window will affect the preferred width and height of future Playground windows'.
	


