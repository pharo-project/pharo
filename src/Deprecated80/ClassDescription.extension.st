Extension { #name : #ClassDescription }

{ #category : #'*Deprecated80' }
ClassDescription >> classThatDefinesInstanceVariable: instVarName [
	self 
		deprecated: 'use #classThatDefinesInstVarNamed:' 
		transformWith: '`@receiver classThatDefinesInstanceVariable: `@statement' -> '`@receiver classThatDefinesInstVarNamed: `@statement'.
	
	^self classThatDefinesInstVarNamed: instVarName
]
