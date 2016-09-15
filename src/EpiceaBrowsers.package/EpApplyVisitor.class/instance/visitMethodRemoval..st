visitor
visitMethodRemoval: aMethodRemoval

	self 
		behaviorNamed: aMethodRemoval behaviorAffectedName
		ifPresent: [ :behavior | 
			behavior removeSelector: aMethodRemoval selector ]