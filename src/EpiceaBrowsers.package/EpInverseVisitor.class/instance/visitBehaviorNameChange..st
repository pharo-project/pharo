visitor
visitBehaviorNameChange: aChange

	^ EpBehaviorNameChange
			oldName: aChange newName
			newName: aChange oldName
			class: aChange behaviorAffected