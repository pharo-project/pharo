Extension { #name : #ClyVariableReferencesQuery }

{ #category : #'*Calypso-SystemTools-Core' }
ClyVariableReferencesQuery >> decorateResultMethodEditor: aMethodEditor [
	| vars |
	vars := variableQuery execute items.
	
	aMethodEditor selectAnyVariable: (vars collect: [:each | each name])
]
