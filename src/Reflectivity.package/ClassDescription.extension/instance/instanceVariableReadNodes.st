*Reflectivity
instanceVariableReadNodes
	^self methods flatCollect: [ :each | each instanceVariableReadNodes ]