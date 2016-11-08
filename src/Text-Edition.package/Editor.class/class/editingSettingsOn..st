settings
editingSettingsOn: aBuilder 
	<systemsettings> 
	(aBuilder group: #codeEditing) 
		label: 'Text Editing';
		noOrdering;
		description: 'All settings concerned with text editing' ;
		with: [	
			(aBuilder setting: #selectionColor)
				target: UITheme;
				targetSelector: #currentSettings;
				label: 'Selection color'.
			(aBuilder setting: #unfocusedSelectionColor)
				target: UITheme;
				targetSelector: #currentSettings;
				description: 'The color of the selection for unfocused windows' ;
				label: 'Unfocused selection color'.
			(aBuilder setting: #selectionTextColor)
				target: UITheme;
				targetSelector: #currentSettings;
				label: 'Selection text color';
				description: 'The color of the selection text'.
			(aBuilder setting: #useSelectionBar)
				target: TextEditor;
				label: 'Show the colored bar on the current line';
				with: [
					(aBuilder setting: #selectionBarColor)
						target: UITheme;
						targetSelector: #currentSettings;
						label: 'Selection bar color'].
			(aBuilder setting: #useSecondarySelection)
				target: TextEditor;
				label: 'Use the secondary selection';
				with: [
					(aBuilder setting: #secondarySelectionColor)
						target: UITheme;
						targetSelector: #currentSettings;
						label: 'Secondary selection color'.
					(aBuilder setting: #secondarySelectionTextColor)
						target: UITheme;
						targetSelector: #currentSettings;
						label: 'Secondary selection text color'].
			(aBuilder setting: #useFindReplaceSelection)
				target: TextEditor;
				label: 'Use the find and replace selection';
				with: [
					(aBuilder setting: #findReplaceSelectionColor)
						target: UITheme;
						targetSelector: #currentSettings;
						label: 'Find replace selection color'.
					(aBuilder setting: #findReplaceSelectionTextColor)
						target: UITheme;
						targetSelector: #currentSettings;
						label: 'Find replace selection text color'].
			(aBuilder setting: #caseSensitiveFinds) 
				label: 'Case sensitive search';
				target: TextEditor;
		 		description: 'If true, then the "find" command in text will always make its searches in a case-sensitive fashion'.
			(aBuilder setting: #blinkingCursor) 
				label: 'Blinking text cursor';
				target: Editor;
		 		description: 'When true, the text cursor will blink.'.
			(aBuilder setting: #dumbbellCursor) 
				label: 'Dumbbell-shaped text cursor';
				target: Editor;
		 		description: 'When true, the text cursor assumes the shape of a dumbbell, otherwise a vertical bar.'.
			(aBuilder setting: #skipOverMultipleSpaces) 
				label: 'Skip over white space';
				target: Editor;
		 		description: 'When true, the text cursor treats multiple white-space characters as a single space for navigating (moving left/right).'.
			(aBuilder setting: #walkAlongDisplayedLine) 
				label: 'Walk along displayed line';
				target: TextEditor;
		 		description: 'If set to true cursor movement will move along displayed lines rather than logical lines'.
		]
