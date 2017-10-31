private
assertResultOfRule: critiques hasSameClassesAsEnvironment: anEnvironment
	self assertResult: critiques hasAllClassesInEnvironment: anEnvironment.
	self assertEnvironment: anEnvironment hasAllClassesOfTheResult: critiques