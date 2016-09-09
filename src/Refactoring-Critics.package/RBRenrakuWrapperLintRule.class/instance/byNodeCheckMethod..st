running
byNodeCheckMethod: aMethod

	aMethod ast nodesDo: [ :node |
		renrakuRule
			check: node
			forCritiquesDo: [ :crit |
				result addMethod: aMethod.
				self
					addSearchStringFrom: crit
					in: aMethod definition.
				^ self ] ]
	