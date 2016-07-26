manifest
falsePositiveOf: aRule
	
	|critics rId rV  mb |
	critics := self criticsOf: aRule. 
	rId := aRule class uniqueIdentifierName.
	rV := aRule class identifierMinorVersionNumber.
	^ critics select: [ :critic | mb := self manifestBuilderOf: critic.
						mb isNil 
							ifTrue: [ false ] 
							ifFalse: [ mb isFalsePositive: critic onRule:  rId version: rV ]]
	