visitor
visitMethodRemoval: aChange

	self 
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior |
			"If method comes from trait, removing has no impact."
			^ behavior 
				compiledMethodAt: aChange methodAffectedSelector
				ifPresent: [ :method | method isFromTrait not ]
				ifAbsent: [ false ] ].

	^ false "No impact if container does not exist"
