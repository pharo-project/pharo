visitor
visitTraitRemoval: aChange

	self 
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior | ^ true ].

	^ false