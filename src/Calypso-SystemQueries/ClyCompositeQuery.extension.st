Extension { #name : #ClyCompositeQuery }

{ #category : #'*Calypso-SystemQueries' }
ClyCompositeQuery >> withMetaLevelScope: aScopeClass [ 
	| newSubqueries |
	newSubqueries := subqueries collect: [ :each | 
		each withMetaLevelScope: aScopeClass ].
	^self class with: newSubqueries as: requiredResult
]
