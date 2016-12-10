settings
settingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #docCommentEnabled)
		target: self;
		description:
			'Allow styled and executable example expressions within method comments. 
		Example: 
		"3+4 >>> 7."';
		label: 'Pharo doc comments';
		parent: #codeBrowsing