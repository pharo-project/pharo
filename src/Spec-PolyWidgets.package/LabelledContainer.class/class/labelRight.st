specs
labelRight
	<spec>
	^ SpecLayout composed
		newRow: [ :row | 
			row
				add: #subwidget origin: 0 @ 0 corner: 0.8 @ 1;
				add: #label origin: 0.8 @ 0 corner: 1 @ 1 ];
		yourself