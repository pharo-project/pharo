private-checking
checkForMissingDependencies: aCompiledMethod
	[ aCompiledMethod gtExamples do: [ :example | 
		example arguments do: [ :dependency | 
			dependency exists 
				ifFalse: [ self notifyForMissingDependency: dependency example: example ] ] ] ]
		on: Error
		do: [ :exception | " ignore " ]