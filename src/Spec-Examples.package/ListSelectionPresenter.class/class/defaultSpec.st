specs
defaultSpec

	^ SpecLayout composed
		newColumn: [ :column |
			column 
				add: #listModel;
				newRow: [ :row |
					row
						add: #textModel1;
						add: #textModel2 ] ];
		yourself