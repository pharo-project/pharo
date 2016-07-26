visitor
visitMethodAddition: aMethodCreated

	self 
		behaviorNamed: aMethodCreated behaviorAffectedName
		ifPresent: [ :behavior | 
			behavior
				compile: aMethodCreated sourceCode 
				classified: aMethodCreated protocol ]