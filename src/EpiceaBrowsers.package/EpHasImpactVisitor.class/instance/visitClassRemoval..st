visitor
visitClassRemoval: aChange

	self 
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior | ^ true ].

	^ false