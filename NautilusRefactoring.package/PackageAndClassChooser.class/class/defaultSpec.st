specs
defaultSpec
	<spec: #default>
	
	^ SpecLayout composed
		newColumn: [ :c |
			c 
				add: #searchField height: 25;
				add: #tree
		];
		yourself