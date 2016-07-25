specs
defaultSpec
	<spec>
	
	 ^ SpecLayout composed
		newRow: [ :r |
			r 
				add: #sideRuler width: 50;
				add: #text ];
		yourself