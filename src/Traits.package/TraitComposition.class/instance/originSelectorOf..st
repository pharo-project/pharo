alias
originSelectorOf: aSelector
 
	self isEmpty ifTrue: [ ^ aSelector ].
	self transformations do:[ :transformation | 
		transformation allAliasesDict 
			at: aSelector ifPresent: [:value | ^ value ]].
		
	^aSelector