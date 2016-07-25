specs
defaultSpec
	<spec: #default>
	
	^ SpecLayout composed
		newColumn: [ :col | 
			col 
				newRow: [ :r | 
					r 
						add: #(viewer models);
					 	newColumn: [ :c | 
							c 
								add: #(viewer protocols);
								add: #(viewer events) ] ];
				add: #text
		];
		yourself