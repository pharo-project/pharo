Extension { #name : #ClyBrowserItem }

{ #category : #'*Calypso-SystemQueries' }
ClyBrowserItem >> ensureSystemDefinition: aBlock [
	
	self getProperty: ClyItemDefinitionProperty ifAbsent: [ 
		self addProperty: aBlock value]
]

{ #category : #'*Calypso-SystemQueries' }
ClyBrowserItem >> systemDefinition [
	
	^self 
		getProperty: ClyItemDefinitionProperty 
		ifAbsent: [ self error: 'No system definition is specified for item' ].
]

{ #category : #'*Calypso-SystemQueries' }
ClyBrowserItem >> systemDefinitionClass [
	
	^self systemDefinition definingClass
]
