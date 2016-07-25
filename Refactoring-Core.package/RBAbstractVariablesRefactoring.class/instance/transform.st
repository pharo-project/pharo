transforming
transform
	self hasVariablesToAbstract 
		ifTrue: 
			[self 
				refactoringWarning: 'This method has direct variable references which<n>will need to be converted to getter/setters.' 
						expandMacros].
	self abstractInstanceVariables.
	self abstractClassVariables