visitor
visitTraitChange: aChange

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :aTrait |
			^ { EpTraitModification oldTrait: aTrait newTrait: aChange behaviorAffected } ].
	
	^ { EpTraitAddition trait: aChange behaviorAffected }