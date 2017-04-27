accessing
spec
	<spec: #default>

	^ SpecLayout composed newColumn: [:column |
		column add: #logNodesTreeModel.

		column 
			newRow: [ :row |
				 row 
					add: #hiedraCheckBoxModel;
					add: #refreshButtonModel" width: self inputTextHeight";
					add: #selectDirectoryButtonModel" width: self inputTextHeight";
					add: #openSettingsButtonModel;
					yourself ]
			 height: self buttonHeight - 4 ].