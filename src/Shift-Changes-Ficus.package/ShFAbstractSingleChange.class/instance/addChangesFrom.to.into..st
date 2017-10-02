checking change
addChangesFrom: oldDefinition to: newDefinition into: results
	(self hasChanged: oldDefinition with: newDefinition)
		ifTrue: [ self fillFrom: oldDefinition to: newDefinition.
			results add: self ]