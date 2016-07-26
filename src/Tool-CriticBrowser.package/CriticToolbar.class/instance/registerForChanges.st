initialization
registerForChanges

	criticHolder whenChangedDo: [ :contents |
		| boolean |
		boolean := contents notNil.
		browseModel state: (boolean and: [ browseModel enabled ]).
		transformModel state: (boolean and: [ transformModel enabled ]).
		falsepositiveModel state: (boolean and: [ falsepositiveModel enabled ]) 
		]
