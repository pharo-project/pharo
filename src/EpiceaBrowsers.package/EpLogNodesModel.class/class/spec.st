accessing
spec
	<spec: #default>

	^ SpecLayout composed newColumn: [:column |
		column 
			newRow: [ :row |
				 row 
					add: #directoryTextModel;
					add: #selectDirectoryButtonModel width: self inputTextHeight;
					add: #refreshButtonModel width: self inputTextHeight ]
			 height: self inputTextHeight - 4.

		column
			add: #logNodesTreeModel ].