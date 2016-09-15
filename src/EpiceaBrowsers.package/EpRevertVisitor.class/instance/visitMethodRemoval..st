visitor
visitMethodRemoval: aMethodRemoved

	self 
		behaviorNamed: aMethodRemoved behaviorAffectedName
		ifPresent: [ :behavior | 
			behavior
				compile: aMethodRemoved sourceCode 
				classified: aMethodRemoved protocol ]