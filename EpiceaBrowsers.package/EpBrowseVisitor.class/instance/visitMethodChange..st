visitor
visitMethodChange: aChange

	self 
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior | 
			behavior 
				compiledMethodAt: aChange methodAffectedSelector 
				ifPresent: #browse 
				ifAbsent: [ 
					self inform: 
						'Cannot find ', behavior asString, '>>', aChange methodAffectedSelector.
					behavior browse ] ].