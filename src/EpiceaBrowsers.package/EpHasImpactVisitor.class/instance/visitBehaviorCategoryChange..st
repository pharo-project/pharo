visitor
visitBehaviorCategoryChange: aChange

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior |
			^ behavior category ~= aChange behaviorAffected category ].
			
	^ true