specs
horizontal
	<spec>
	^ SpecLayout composed
		newRow: [ :row | 
			row
				add: #firstButton;
				add: #secondButton ];
		yourself