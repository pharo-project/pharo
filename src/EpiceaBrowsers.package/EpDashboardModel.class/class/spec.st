specs
spec
	<spec: #default>

	^SpecLayout composed newColumn: [:column |
		column add: #currentSessionInfoModel.
		
		column
			newRow: [ :row |
				row
					add: #enableMonitorButtonModel;
					add: #createLogButtonModel;
					add: #openSettingsButtonModel;
					yourself ]
			height: self buttonHeight. 
		]