specs
layout
	<spec>
	^ SpecLayout composed
		newColumn: [ :column | column add: #screen ]origin: 0 @ 0 corner: 0.2 @ 1;
		newColumn: [ :column | 
			column
				add: #buttonHappy;
				add: #buttonNeutral;
				add: #buttonBad ] origin: 0.2 @ 0 corner: 1 @ 1;
		yourself