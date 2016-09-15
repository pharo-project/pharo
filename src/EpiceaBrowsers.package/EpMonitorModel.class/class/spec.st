accessing
spec
	<spec: #default>

	^ SpecLayout composed newColumn: [:column |
		column 
			newRow: [ :row |
				 row 
					add: #enableMonitorButtonModel width: 95;
					add: #activeLogLabelModel width: 75;
					add: #activeLogTextModel;
					add: #createActiveLogButtonModel width: self inputTextHeight;
					add: #logSetButtonModel width: 75;
					yourself
					 ]
			 height: self inputTextHeight - 4.

		column
			add: #logModel ].