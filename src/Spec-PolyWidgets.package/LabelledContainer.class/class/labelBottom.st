specs
labelBottom
	<spec>
	^ SpecLayout composed
		newColumn: [ :column | 
			column
				add: #subwidget origin: 0 @ 0 corner: 1 @ 0.9;
				add: #label origin: 0 @ 0.9 corner: 1 @ 1 ];
		yourself