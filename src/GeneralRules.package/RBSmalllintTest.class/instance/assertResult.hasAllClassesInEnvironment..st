asserting
assertResult: critiques hasAllClassesInEnvironment: anEnvironment
	anEnvironment classesDo: [ :class |
		self assert: (critiques anySatisfy: [ :crit |
			crit sourceAnchor entity theNonMetaClass = class theNonMetaClass ]) ]