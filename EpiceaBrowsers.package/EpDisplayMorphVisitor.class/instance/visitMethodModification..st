visitor
visitMethodModification: aMethodModified
	^ self 
		displayBehavior: aMethodModified behaviorAffectedName
		method: aMethodModified oldMethod selector