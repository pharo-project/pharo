settings
rubricSettingsOn: aBuilder
	<systemsettings>
	(aBuilder group: #'Rubric cursor')
		parent: #Rubric;
		label: 'Cursor';
		with: [ 
					(aBuilder setting: #cursorWithFixedColor)
						target: self;
						label: 'Fixed color';
						with: [ 
									(aBuilder setting: #defaultColor)
										target: self;
										description: 'The cursolr color color';
										label: 'Color'  ].
					(aBuilder range: #defaultWidth)
						target: self;
						label: 'Width';
						description: 'Specify the cursor width';
						range: (1 to: 4).
					(aBuilder range: #period)
						target: self;
						label: 'Blinking period';
						description:
								'Specify the cursor blinking period in milliseconds. If it is set to zero then the cursor will not blink';
						range: (0 to: 1000) ]