private
assertEnvironment: anEnvironment hasAllClassesOfTheResult: critiques

	critiques do: [ :crit |
		| target |
		target := crit sourceAnchor entity.
		self assert: target isClass.
		self assert: (anEnvironment includesClass: target) ]