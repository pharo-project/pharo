manifest
falsePositiveOf: aRule
	
	| ruleCritiques rId rV  mb |
	ruleCritiques := self criticsOf: aRule. 
	rId := aRule class uniqueIdentifierName.
	rV := aRule class identifierMinorVersionNumber.
	^ ruleCritiques select: [ :critique |
		| entity |
		entity := critique sourceAnchor entity.		
		mb := self manifestBuilderOf: entity.
		mb isNil 
			ifTrue: [ false ] 
			ifFalse: [ mb isFalsePositive: entity onRule:  rId version: rV ]]
	