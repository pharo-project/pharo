transforming
compileNewMethods
	moveToClasses 
		do: [:each | each compile: parseTree newSource withAttributesFrom: (class methodFor: selector)]