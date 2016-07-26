settings
nautilusSettingsOn: aBuilder
	<systemsettings>
	(aBuilder group: #codeBrowsing)
		label: 'Code Browsing';
		description: 'All settings concerned with code browsing look''n feel';
		with:
				[ (aBuilder group: #Nautilus)
				target: self;
				label: 'Nautilus Browser';
				parent: #codeBrowsing;
				description: 'Settings related to the browser';
				with:
						[ (aBuilder setting: #sortHierarchically)
						label: 'Show hierarchy by default ?';
						description: 'If false, Nautilus will show a flat list of classes'.
					(aBuilder setting: #useOldStyleKeys)
						label: 'Use old style keys ?';
						description: 'If true, the browser will use the old style keys (This is highly non-recommended, you will lose a lot of cool access keys)' ] ]