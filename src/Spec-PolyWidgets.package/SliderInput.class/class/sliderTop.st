specs
sliderTop
	<spec>
	^ SpecLayout composed
		newColumn: [ :column | 
			column
				add: #slider origin: 0 @ 0 corner: 1 @ 0.85;
				add: #input origin: 0 @ 0.85 corner: 1 @ 1 ];
		yourself