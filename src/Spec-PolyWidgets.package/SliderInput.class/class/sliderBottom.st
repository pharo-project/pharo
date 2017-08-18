specs
sliderBottom
	<spec>
	^ SpecLayout composed
		newColumn: [ :column | 
			column
				add: #input origin: 0 @ 0 corner: 1 @ 0.15;
				add: #slider origin: 0 @ 0.15 corner: 1 @ 1 ];
		yourself