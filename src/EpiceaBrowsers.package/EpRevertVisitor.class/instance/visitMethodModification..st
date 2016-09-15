visitor
visitMethodModification: aMethodModified

	self 
		behaviorNamed: aMethodModified behaviorAffectedName
		ifPresent: [ :behavior | 
			behavior
				compile: aMethodModified oldSourceCode 
				classified: aMethodModified oldProtocol ]