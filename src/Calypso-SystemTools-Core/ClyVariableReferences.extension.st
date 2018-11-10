Extension { #name : #ClyVariableReferences }

{ #category : #'*Calypso-SystemTools-Core' }
ClyVariableReferences >> decorateResultMethodEditor: aMethodEditor [
	| vars |
	vars := variableQuery execute items.
	
	aMethodEditor selectAnyVariable: (vars collect: [:each | each name])
]
