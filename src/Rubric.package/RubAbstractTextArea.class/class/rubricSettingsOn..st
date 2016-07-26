settings
rubricSettingsOn: aBuilder
	<systemsettings>
	(aBuilder group: #Rubric)
		label: 'Rubric editor';
		parent: #codeEditing;
		with: [ 
					(aBuilder setting: #backgroundColor)
						target: self;
						description: 'The default editor background color';
						label: 'Background'.
					(aBuilder setting: #textColor)
						target: self;
						description: 'The default editor text color';
						label: 'Text color'.
					(aBuilder group: #LineNumbers)
						target: self;
						label: 'Line numbers';
						description: 'line numbers settings';
						with: [ 
									(aBuilder setting: #lineNumbersFont)
										target: self;
										description: 'Font used for line numbers';
										label: 'Line numbers font'.
									(aBuilder setting: #lineNumbersTextColor)
										target: self;
										description: 'Color used for line numbers';
										label: 'Line numbers color'.
									(aBuilder setting: #lineNumbersBackgroundColor)
										target: self;
										description: 'Color used for line numbers background';
										label: 'Line numbers background color' ] ]