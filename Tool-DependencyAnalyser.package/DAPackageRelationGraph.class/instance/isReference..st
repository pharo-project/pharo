testing
isReference: aLiteral
	^ aLiteral isVariableBinding and: [ aLiteral value isBehavior and: [ aLiteral key = aLiteral value name ] ]