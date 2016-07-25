visitor
visitMethodRemoval: aMethodRemoved
	^ self 
		displayBehavior: aMethodRemoved behaviorAffectedName
		method: aMethodRemoved methodRemoved selector