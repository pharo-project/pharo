morphic
morphicWindowsSettingsOn: aBuilder 
	(aBuilder group: #windows)
		label: 'Windows';
		description: 'All windows settings';
		noOrdering;
		with: [
			(aBuilder setting: #preferRoundCorner)
				target: UITheme;
				targetSelector: #currentSettings;
				label: 'Rounded corners';
				description: 'If true, then windows, menu and other popups will have their corners rounded.'.
			(aBuilder range: #fullscreenMargin) 
						target: SystemWindow;
						label: 'Fullscreen margin';
						description: 'Specify the amount of space that is let around a windows when it''s opened fullscreen';
						range: (-5 to: 100).
			(aBuilder setting: #fadedBackgroundWindows)
				target: UITheme;
				targetSelector: #currentSettings;
				label: 'Fade background windows';
				description: 'If true then, background windows appear more "washed out" to distinguish from the active window'.
			(aBuilder setting: #fastDragging)
				target: UITheme;
				targetSelector: #currentSettings;
				label: 'Fast dragging';
				description: 'If true, window drag will be done by dragging an outline of the window instead of the whole window. It is very convenient on a slow machine' .
			(aBuilder range: #textEntryFieldMinimumWidth)
				label: 'Text entry dialog width';
				target: TextEntryDialogWindow;
				selector: #minimumWidth;
				range: (100 to: 800);
				description: 'Set the default text field width of the text entry dialog window' ]
			
			
			
