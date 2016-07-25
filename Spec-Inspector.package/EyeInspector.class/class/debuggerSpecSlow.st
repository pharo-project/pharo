spec
debuggerSpecSlow
	<spec>
	
	^ SpecLayout composed
		newRow: [:row | 
			row 
				add: #list width: 135;
				add: #description ];
		yourself