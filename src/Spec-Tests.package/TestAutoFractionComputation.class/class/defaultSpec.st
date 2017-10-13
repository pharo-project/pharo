spec
defaultSpec
	<spec>
	
	^ SpecLayout composed
		newRow: [ :r |
			r 
				add: #list left: 0.8;
				newColumn: [ :c |
					c 
						add: #button bottom: 0.7;
						add: #text top: 0.3 ] right: 0.2 ];
		yourself