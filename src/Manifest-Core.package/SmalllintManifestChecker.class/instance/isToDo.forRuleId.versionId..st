manifest
isToDo: aCritic forRuleId: ruleId versionId: versionId 
	
	| mb |
	mb := self manifestBuilderOf: aCritic.
	^  mb isNil 
		ifTrue: [ false ] 
		ifFalse: [ mb containsToDo: aCritic onRule:  ruleId version: versionId ]
