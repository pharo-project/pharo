specs
defaultSpec
	<spec: #default>
	^ SpecLayout composed
		newColumn: [ :column | 
			column 
				add: #text height: 25;
				newRow: [ :row | 
					row
						add: #minus;
						addSplitter;
						add: #plus ]
					height: 25 ];
		yourself