Extension { #name : #ClyCompositeScope }

{ #category : #'*Calypso-SystemQueries' }
ClyCompositeScope >> asInheritedScope [
	| newSubscopes |
	newSubscopes := subscopes collect: [ :each | each asInheritedScope  ].
	^ClyCompositeScope on: newSubscopes in: environment named: ClyClassScope inheritedScopeName
]

{ #category : #'*Calypso-SystemQueries' }
ClyCompositeScope >> asInheritingScope [
	| newSubscopes |
	newSubscopes := subscopes collect: [ :each | each asInheritingScope  ].
	^ClyCompositeScope on: newSubscopes in: environment named: ClyClassScope inheritingScopeName
]

{ #category : #'*Calypso-SystemQueries' }
ClyCompositeScope >> asInterestingClassScope [
	| newSubscopes |
	newSubscopes := subscopes collect: [ :each | each asInterestingClassScope ].
	^ClyCompositeScope on: newSubscopes in: environment named: name
]

{ #category : #'*Calypso-SystemQueries' }
ClyCompositeScope >> asLocalClassScope [
	| newSubscopes |
	newSubscopes := subscopes collect: [ :each | each asLocalClassScope ].
	^ClyCompositeScope on: newSubscopes in: environment
]

{ #category : #'*Calypso-SystemQueries' }
ClyCompositeScope >> asRBEnvironment [
	| env |
	env := subscopes first asRBEnvironment.
	subscopes allButFirstDo: [:each | env := env | each asRBEnvironment  ].
	^env
]

{ #category : #'*Calypso-SystemQueries' }
ClyCompositeScope >> classesDo: aBlock [ 
	subscopes do: [ :each | each classesDo: aBlock]
]

{ #category : #'*Calypso-SystemQueries' }
ClyCompositeScope >> collectAllClasses [
	| result |
	result := IdentitySet new.
	
	subscopes do: [ :scope | scope classesDo: [:each | result add: each ]].
	
	^result
]

{ #category : #'*Calypso-SystemQueries' }
ClyCompositeScope >> packagesDo: aBlock [ 
	subscopes do: [ :each | each packagesDo: aBlock]
]

{ #category : #'*Calypso-SystemQueries' }
ClyCompositeScope >> withMetaLevel: aScopeClass [ 
	| newSubscopes |
	newSubscopes := subscopes collect: [ :each | 
		each withMetaLevel: aScopeClass ].
	^ClyCompositeScope on: newSubscopes in: environment named: name
]
