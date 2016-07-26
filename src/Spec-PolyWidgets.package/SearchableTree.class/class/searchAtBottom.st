specs
searchAtBottom
	<spec: #default>
	
	^ SpecLayout composed
		newColumn: [:c |
			c 
				add: #tree;
				newRow: [:r |
					r
					"add: #label width: 50;"
					add: #inputField;
					add: #button width: 25 ] height: 25 ];
		yourself