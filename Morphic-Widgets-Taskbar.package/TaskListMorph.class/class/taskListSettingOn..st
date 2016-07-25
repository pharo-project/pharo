settings
taskListSettingOn: aBuilder
	<systemsettings> 
	(aBuilder setting: #keepTaskListOpen)
		label: 'Keep task list open';
		description: 'Whether the tasklist is closed (and the selected window activated) when the command key is released';
		parent: #morphic;
		selector: #keepOpen;
		target: self.