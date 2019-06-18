"
I am abstract query of all methods from given scope.
Subclasses should define concrete condition which all retrieved methods should satisfy: 
- selectsMethod: aMethod 

Scope should support #methodsDo: which I use to collect and filter all available methods.
"
Class {
	#name : #ClyMethodQuery,
	#superclass : #ClyTypedQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #'items type' }
ClyMethodQuery class >> resultItemsType [
	^ClyMethod
]

{ #category : #execution }
ClyMethodQuery >> buildResult: aQueryResult [
	| filteredMethods |
	filteredMethods := OrderedCollection new.
	
	scope methodsDo: [ :each | 
		(self selectsMethod: each) ifTrue: [filteredMethods add: each]].
	
	aQueryResult fillWith: filteredMethods
]

{ #category : #execution }
ClyMethodQuery >> checkEmptyResult [

	scope methodsDo: [ :each | 
		(self selectsMethod: each) ifTrue: [^false]].
	
	^true
]

{ #category : #execution }
ClyMethodQuery >> collectMetadataOf: aQueryResult by: anEnvironmentPlugin [
	anEnvironmentPlugin collectMetadataOfMethods: aQueryResult
]

{ #category : #'system changes' }
ClyMethodQuery >> isAffectedByChangedMethod: aMethod [
	
	^self selectsMethod: aMethod
]

{ #category : #'system changes' }
ClyMethodQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [
	
	^(aSystemAnnouncement canAffectResultOfMethodQuery: self)
		and: [ scope includesMethodsAffectedBy: aSystemAnnouncement]
]

{ #category : #testing }
ClyMethodQuery >> retrievesItem: aMethod [
	^self selectsMethod: aMethod
]

{ #category : #testing }
ClyMethodQuery >> selectsMethod: aMethod [
	self subclassResponsibility 
]
