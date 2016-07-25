settings
settingInputWidgetForNode: aSettingNode 
	| theme |
	theme := UITheme builder.
	^ theme newRow: {theme
			newColorChooserFor: aSettingNode realValue
			getColor: #color
			setColor: #color:
			help: 'Choose token color'. (theme
			newDropListFor: aSettingNode realValue
			list: #(#bold #italic #normal 'bold italic' )
			getSelected: #emphasis
			setSelected: #emphasis:
			getEnabled: nil
			useIndex: false
			help: 'Choose token emphasis') hResizing: #rigid;
			 width: 100}