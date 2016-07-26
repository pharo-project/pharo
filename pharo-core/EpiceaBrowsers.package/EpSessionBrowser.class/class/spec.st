accessing
spec
	<spec: #default>

	^ SpecLayout composed newColumn: [:column |
		column 
			newRow: [ :row |
				 row 
					add: #statusBarModel;
					add: #selecteDirectoryButtonModel width: 50 ]
			 height: self buttonHeight.

		column
			add: #sessionsTreeModel.
			
		column 
			newRow: [ :row |
				 row 
					add: #openButtonModel;
					add: #refreshButtonModel ] 
			height: self buttonHeight ].