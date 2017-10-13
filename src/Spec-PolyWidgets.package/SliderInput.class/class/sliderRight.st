specs
sliderRight
	<spec>
	^ SpecLayout composed
		newRow: [ :row | 
			row
				add: #input origin: 0 @ 0 corner: 0.15 @ 1;
				add: #slider origin: 0.15 @ 0 corner: 1 @ 1 ];
		yourself