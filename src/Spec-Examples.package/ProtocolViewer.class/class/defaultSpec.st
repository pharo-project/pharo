specs
defaultSpec
	<spec: #default>
	
	^ SpecLayout composed
		newColumn: [ :column |
			column 
				add: #models; 
				add: #protocols; 
				add: #events ];
		yourself