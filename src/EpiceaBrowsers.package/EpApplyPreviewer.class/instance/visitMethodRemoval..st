visitor
visitMethodRemoval: aChange

	self 
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior |
			"If method comes from trait, removing has no impact."
			^ behavior 
				compiledMethodAt: aChange methodAffectedSelector
				ifPresent: [ :method | method isFromTrait not
					ifTrue: [ { EpMethodRemoval method: method } ]
					ifFalse: [ #() ] ]
				ifAbsent: [ #() ] ].

	^ #() "No impact if container does not exist"
