visitor
visitBehaviorChange: aChange

	self 
		behaviorNamed: aChange behaviorAffectedName 
		ifPresent: #browse