specs
defaultSpec
	<spec: #default>
	
	^ SpecLayout composed 
		newColumn: [ :col | 
			col newRow: [ :row | 
					row newColumn: [ :subcolumn |
						subcolumn add: #packages.
						subcolumn add: #showClassSide height: 16  ].
					row 
						add: #classes;
						add: #protocols;
						add: #selectors ] ;
			newRow: [ :row | 
					row add: #method ]]; 
		yourself