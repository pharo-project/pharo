visitor
visitClassRemoval: aChange

	self 
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior | ^ { EpClassRemoval class: behavior } ].

	^ #()