testing
doesOverride: aCodeChange 
	
	^ aCodeChange class = self class and: [ 
		aCodeChange behaviorAffectedName = self behaviorAffectedName ]