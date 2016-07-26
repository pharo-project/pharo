setting
taskbarSettingOn: aBuilder
	<systemsettings>
	(aBuilder setting: #showTaskbar)
		target: self;
		parent: #morphic;
		label: 'Taskbar';
		description: 'Whether the world''s taskbar should be shown or not.';
		with: [
			(aBuilder setting: #showWindowPreview)
				label: 'Window preview' ;
				description: 'Whether the world''s taskbar buttons should show previews of the associated window while the mouse is over them.'.
			(aBuilder setting: #maximumButtons)
				label: 'Maximum number of tasks to show';
				description: ('By default, the value is ', self maximumButtonsDefault printString).

		]