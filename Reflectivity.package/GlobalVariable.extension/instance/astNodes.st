*Reflectivity
astNodes
	^self usingMethods flatCollect: [ :method | 
		method variableNodes select: [: each| each binding variable == self ]]