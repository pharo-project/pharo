specs
defaultSpec
	<spec: #default>
	
	^ SpecLayout composed 
		newColumn: [ :col | 
			col
				add: #changes;
				newRow: [ :row | 
					row
						add: #selectAllButton;
						add: #deselectAllButton;
						add: #fileInSelectedButton ] height: 32;
				add: #codePane ]; 
		yourself