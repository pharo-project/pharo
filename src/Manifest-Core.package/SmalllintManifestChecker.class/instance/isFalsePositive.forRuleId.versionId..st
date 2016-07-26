manifest
isFalsePositive: aCritic forRuleId: ruleId versionId: versionId
	| mb |
	mb := self manifestBuilderOf: aCritic.
	^  mb isNil 
		ifTrue: [ false ] 
		ifFalse: [ mb isFalsePositive: aCritic onRule: ruleId version: versionId ]
