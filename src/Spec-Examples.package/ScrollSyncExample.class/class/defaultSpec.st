specs
defaultSpec
	<spec>
	^ SpecLayout composed
		newRow: [ :row | 
			row
				add: #sideRuler width: 50;
				add: #text ];
		yourself