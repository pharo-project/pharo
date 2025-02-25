"
Query class to return references to a given shared pool.

And in addition I provide more readable methods to instantiate my instances from classes:

```smalltalk
	ClySharedPoolReferencesQuery to: aSharedPool.
	ClySharedPoolReferencesQuery to: aSharedPool from: aScope.
	ClySharedPoolReferencesQuery toAny: {Array. String}.
	ClySharedPoolReferencesQuery toAny: {Array. String} from: aScope.
```
"
Class {
	#name : 'ClySharedPoolReferencesQuery',
	#superclass : 'ClyClassReferencesQuery',
	#category : 'Calypso-SystemQueries-Queries',
	#package : 'Calypso-SystemQueries',
	#tag : 'Queries'
}

{ #category : 'execution' }
ClySharedPoolReferencesQuery >> buildResult: aQueryResult [

	| filteredClasses |

	filteredClasses := OrderedCollection new.

	scope classesDo: [ : aClass |
		(self selectClass: aClass)
			ifTrue: [ filteredClasses add: aClass ] ].

	aQueryResult fillWith: filteredClasses
]

{ #category : 'execution' }
ClySharedPoolReferencesQuery >> checkEmptyResult [

	scope classesDo: [ :each |
		(self selectClass: each) 
				ifTrue: [ ^ false ] ].
	^ true
]

{ #category : 'execution' }
ClySharedPoolReferencesQuery >> selectClass: aClass [

	^ aClass hasSharedPools and: [ aClass includesSharedPoolNamed: self sharedPoolName ]
]

{ #category : 'execution' }
ClySharedPoolReferencesQuery >> sharedPoolName [

	| vars |
	vars := variableQuery execute items.
	^ vars anyOne value name
]
