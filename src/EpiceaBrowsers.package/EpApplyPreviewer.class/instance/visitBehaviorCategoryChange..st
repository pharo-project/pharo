visitor
visitBehaviorCategoryChange: aChange

	self
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior |
			^ behavior category = aChange behaviorAffected category
				ifTrue: [ #() ]
				ifFalse: [ {
					EpBehaviorCategoryChange
						oldCategory: aChange oldCategory
						newCategory: aChange newCategory
						class: behavior 
					}
				]
			].
			
	^ #()