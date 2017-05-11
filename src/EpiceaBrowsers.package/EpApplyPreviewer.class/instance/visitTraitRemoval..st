visitor
visitTraitRemoval: aChange

	self 
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior | ^ { EpTraitRemoval trait: behavior } ].

	^ #()