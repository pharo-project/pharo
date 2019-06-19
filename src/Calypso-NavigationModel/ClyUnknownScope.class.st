"
I am kind of null object for environment scope.

I am default scope of any query. I provide #instance singleton for this.

Also I prevent real execution of query by returning ClyUnknownResult instance from #query method. 
So environment is not requested to evaluate given query when query is bound to me.
"
Class {
	#name : #ClyUnknownScope,
	#superclass : #ClyScope,
	#classInstVars : [
		'instance'
	],
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #accessing }
ClyUnknownScope class >> instance [
	^instance ifNil: [ instance := ClyUnknownScope new ]
]

{ #category : #accessing }
ClyUnknownScope class >> reset [
	instance := nil
]

{ #category : #queries }
ClyUnknownScope >> adoptQuery: aQuery [ 
	^aQuery
]

{ #category : #queries }
ClyUnknownScope >> asScope: anotherEnvironmentScopeClass [

	^self
]

{ #category : #converting }
ClyUnknownScope >> asUnifiedInstance [
	^self
]

{ #category : #initialization }
ClyUnknownScope >> initialize [
	super initialize.
	
	basisObjects := #()
]

{ #category : #testing }
ClyUnknownScope >> isQueryEmpty: aQuery [
	^true
]

{ #category : #queries }
ClyUnknownScope >> query: aQuery [
	^ClyUnknownQueryResult instance
]

{ #category : #testing }
ClyUnknownScope >> supportsQuery: aQuery [
	^true
]

{ #category : #converting }
ClyUnknownScope >> withNewBasisObjects: aCollection [ 
	^self
]
