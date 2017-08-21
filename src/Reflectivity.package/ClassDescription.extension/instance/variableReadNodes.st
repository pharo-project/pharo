*Reflectivity
variableReadNodes
	^self methods flatCollect: [ :each | each variableReadNodes ]