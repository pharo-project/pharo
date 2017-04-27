visitor
visitBehaviorCategoryChange: aChange

	^ EpBehaviorCategoryChange
			oldCategory: aChange newCategory
			newCategory: aChange oldCategory
			class: aChange behaviorAffected
