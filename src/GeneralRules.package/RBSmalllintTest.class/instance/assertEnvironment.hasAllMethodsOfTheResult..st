private
assertEnvironment: anEnvironment hasAllMethodsOfTheResult: critiques

	critiques do: [ :crit |
		| target |
		target := crit sourceAnchor entity.
		self assert: target isCompiledMethod.
		self assert: (anEnvironment includesMethod: target) ]