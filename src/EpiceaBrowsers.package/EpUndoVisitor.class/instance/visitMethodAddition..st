visitor
visitMethodAddition: aMethodCreated

	self 
		behaviorNamed: aMethodCreated behaviorAffectedName
		ifPresent: [ :behavior | 
			behavior removeSelector: aMethodCreated selector ]