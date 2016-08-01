specs
bottomLayout
	<spec>
	^ SpecLayout composed
		newColumn: [ :column | 
			column
				newRow: [ :row | 
					row
						add: #input;
						add: #button ]
					height: 25;
				newRow: [ :row | 
					row
						add: #label;
						add: #check width: 100 ]
					height: 25 ];
		yourself