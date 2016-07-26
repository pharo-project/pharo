manifest
rejectRulesOf: aPackage
		
	| mb |
	mb := self manifestBuilderOf: aPackage.
	^  mb isNil 
		ifTrue: [{}] 
		ifFalse: [ mb rejectRules]
	