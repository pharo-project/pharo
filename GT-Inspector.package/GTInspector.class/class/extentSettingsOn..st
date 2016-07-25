settings
extentSettingsOn: aBuilder 
	<systemsettings>
	
	(aBuilder setting: #GTInspectorPreferredWidth)
		target: self;
		parent: #gt;
		getSelector: #preferredWidth;
		setSelector: #preferredWidth:; 
		label: 'Inspector width';
		description: 'The preferred width of new Inspector windows'.
	(aBuilder setting: #GTInspectorPreferredHeight)
		target: self;
		parent: #gt;
		getSelector: #preferredHeight;
		setSelector: #preferredHeight:; 
		label: 'Inspector height';
		description: 'The preferred height of new Inspector windows'.
	(aBuilder setting: #GTInspectorRememberPreferredExtent)
		target: self;
		parent: #gt;
		getSelector: #rememberPreferredExtent;
		setSelector: #rememberPreferredExtent:; 
		label: 'Remember extent of Inspector';
		description: 'Enable or disable remembering of the extent of Inspector windows on resize. When the preference is true, resizing a Inspector window will affect the preferred width and height of future Inspector windows'
