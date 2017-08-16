helpers
validationScenario
	| critiques |
	
	critiques := Set new.

	[ MyTestError signal: self errorMessage ]
		on: Error
		do: [ :er |
			ReExceptionStrategy current
				handle: er
				about: self
				forProppertiesDo: [ :prop |
					critiques add: prop ] ].
			
	^ critiques
	