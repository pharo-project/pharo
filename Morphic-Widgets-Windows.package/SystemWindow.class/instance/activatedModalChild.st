activation
activatedModalChild

	self modalChild ifNotNil: [ :modalChild |
		modalChild owner ifNotNil: [ 
				modalChild activate.
				modalChild modalChild ifNil: [ modalChild indicateModalChild ].
				^true.			
		]
	].
	^false

