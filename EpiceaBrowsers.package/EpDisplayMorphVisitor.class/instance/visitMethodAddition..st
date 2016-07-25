visitor
visitMethodAddition: aMethodCreatedChange
	^ self 
		displayBehavior: aMethodCreatedChange behaviorAffectedName
		method: aMethodCreatedChange methodAdded selector