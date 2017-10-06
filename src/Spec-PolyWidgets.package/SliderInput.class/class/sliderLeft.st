specs
sliderLeft
	<spec: #default>
	^ SpecLayout composed
		newRow: [ :row | 
			row
				add: #slider origin: 0 @ 0 corner: 0.85 @ 1;
				add: #input origin: 0.85 @ 0 corner: 1 @ 1 ];
		yourself