generic parent api
theNonMetaParentName
	"Rejects the prefix ' class' or ' classTrait' of the parentName"
	| index |
	
	index := self parentName
				indexOfSubCollection: ' class'
				startingAt: 1
				ifAbsent: [ ^self parentName ].

	^(self parentName 
		copyFrom: 1
		to: index - 1) asSymbol