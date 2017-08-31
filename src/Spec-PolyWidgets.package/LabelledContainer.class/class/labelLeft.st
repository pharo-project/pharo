specs
labelLeft
	<spec>
	^ SpecLayout composed
		newRow: [ :row | 
			row
				add: #label origin: 0 @ 0 corner: 0.2 @ 1;
				add: #subwidget origin: 0.2 @ 0 corner: 1 @ 1 ];
		yourself