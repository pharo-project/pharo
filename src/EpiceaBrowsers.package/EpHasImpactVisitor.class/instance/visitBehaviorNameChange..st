visitor
visitBehaviorNameChange: aChange

	self 
		behaviorNamed: aChange oldName
		ifPresent: [ :behavior | ^ true ].

	^ false