*Reflectivity
astNodes
	^self definingClass variableNodes select: [ :each | each binding variable == self ]