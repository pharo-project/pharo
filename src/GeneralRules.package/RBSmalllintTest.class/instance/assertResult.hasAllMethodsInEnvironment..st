asserting
assertResult: critiques hasAllMethodsInEnvironment: anEnvironment
	anEnvironment methodsDo: [ :method |
		self assert: (critiques anySatisfy: [ :crit |
			crit sourceAnchor entity = method ]) ]