visitor
visitBehaviorCategoryChange: aChange

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [:aClassOrTrait | aClassOrTrait category: aChange newCategory ]
