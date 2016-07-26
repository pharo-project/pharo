visitor
visitMethodModification: aMethodModified

	self 
		behaviorNamed: aMethodModified behaviorAffectedName
		ifPresent: [ :behavior | 
			behavior
				compile: aMethodModified newSourceCode 
				classified: aMethodModified newProtocol ]