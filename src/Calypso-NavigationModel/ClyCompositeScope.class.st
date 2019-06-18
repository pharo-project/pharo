"
I am scope which merges multiple subscopes. 
I am supposed to represent scope of composite query where real execution logic is delegated to each subquery. 
So am not really participate in query execution. But I am still can be used by UI tools to provide optional scopes for query execution.

To create my instances use following script:

		ClyCompositeScope on: { aScope1. aScope2 }
		
But usually I am created using concatenation message to simple scope: 

	aScope1 , aScope2 
	
I am supposed to be created on the set of typed scopes.

And I implement abstract methods of superclass: 

- adoptQuery: aQuery. It creates ClyUnionQuery with subqueries created from given aQuery with each subscope.
- representsScope: aScopeClass. It ask every subscope if it represents given scope.
- supportsQuery: aQuery. I support only composite queries.

Internal Representation and Key Implementation Points.

    Instance Variables
	subscopes:		<ClyTypedScope>

"
Class {
	#name : #ClyCompositeScope,
	#superclass : #ClyScope,
	#instVars : [
		'subscopes'
	],
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #'instance creation' }
ClyCompositeScope class >> on: subscopes [
	subscopes size = 1 ifTrue: [ ^subscopes anyOne ].
	
	^self new 
		subscopes: subscopes
]

{ #category : #'instance creation' }
ClyCompositeScope class >> on: subscopes in: aNavigationEnvironment [
	^(self on: subscopes)
		bindTo: aNavigationEnvironment
]

{ #category : #'instance creation' }
ClyCompositeScope class >> on: subscopes in: aNavigationEnvironment named: aString [
	^(self on: subscopes in: aNavigationEnvironment)
		name: aString
]

{ #category : #composition }
ClyCompositeScope >> , anotherTypedScope [
	(anotherTypedScope isKindOf: ClyTypedScope) ifFalse: [ 
		^self error: 'Complex composition is not supported' ].
	^ClyCompositeScope on: (subscopes copyWith: anotherTypedScope)
]

{ #category : #queries }
ClyCompositeScope >> adoptQuery: aQuery [
	| subqueries result |
	subqueries := subscopes collect: [ :each | aQuery withScope: each ].
	result := ClyQuery unionFrom: subqueries.
	result scope name: name.
	^result
]

{ #category : #accessing }
ClyCompositeScope >> bindTo: aNavigationEnvironment [
	super bindTo: aNavigationEnvironment.
	
	subscopes do: [ :each | each bindTo: aNavigationEnvironment]
]

{ #category : #printing }
ClyCompositeScope >> printBasisObject: anObject on: aStream [

	| actualScope |
	actualScope := subscopes detect: [ :each | each isBasedOn: anObject ].
	^actualScope printBasisObject: anObject on: aStream
]

{ #category : #printing }
ClyCompositeScope >> printDescriptionOn: aStream [

	name ifNotNil: [ ^super printDescriptionOn: aStream ].
	
	subscopes do: [ :each | 
		each printDescriptionOn: aStream.
		aStream nextPutAll: ', ' ].
	aStream skip: -2 "composite scope must include at least on subscope"
]

{ #category : #testing }
ClyCompositeScope >> representsScope: aScopeClass [
	^subscopes anySatisfy: [ :each | each representsScope: aScopeClass ]
]

{ #category : #accessing }
ClyCompositeScope >> subscopes [
	^ subscopes
]

{ #category : #accessing }
ClyCompositeScope >> subscopes: aCollection [
	aCollection ifEmpty: [ ^self error: 'ClyCompositeScope should include at least one subscope' ].
	
	subscopes := aCollection.
	environment := subscopes anyOne environment.
	self basisObjects: (subscopes flatCollect: [ :each | each basisObjects ])
]

{ #category : #testing }
ClyCompositeScope >> supportsQuery: aQuery [
	^aQuery isKindOf: ClyCompositeQuery
]
