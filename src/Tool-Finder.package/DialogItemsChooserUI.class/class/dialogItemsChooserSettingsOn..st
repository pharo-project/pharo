settings
dialogItemsChooserSettingsOn: aBuilder
	<systemsettings>
		(aBuilder group: #dialogItemsChooser)
		target: self;
		label: 'Items Chooser Dialog Window';
		parent: #morphic;
		description: 'Settings related to the Dialog Window for choosing elements among a list' ;
			with: [
				(aBuilder setting: #alreadySearchedUnselectedItemsListMaxSize)
					label: 'Number of the Unselected Items in History'.
				(aBuilder setting: #alreadySearchedSelectedItemsListMaxSize)
					label: 'Number of the Selected Items in History'.]