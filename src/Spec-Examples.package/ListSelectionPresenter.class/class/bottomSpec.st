specs
bottomSpec

	^ SpecLayout composed
		newRow: [ :row |
			row
				add: #textModel1;
				add: #textModel2 ];
		yourself