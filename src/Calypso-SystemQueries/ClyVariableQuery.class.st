"
I am abstract query of all variables from given scope.

I only implement method related to the retrieved items type.
So my subclasses still should implement main query methods.

And I provide extra convertation method to get similar query but from different meta level class scope:
	
	aVariableQuery withMetaLevelScope: ClyInstanceSideScope
"
Class {
	#name : #ClyVariableQuery,
	#superclass : #ClyTypedQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #'items type' }
ClyVariableQuery class >> resultItemsType [
	^ClyVariable
]

{ #category : #execution }
ClyVariableQuery >> collectMetadataOf: aQueryResult by: anEnvironmentPlugin [
	self flag: #todo
]

{ #category : #'system changes' }
ClyVariableQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [
	
	^scope includesVariablesAffectedBy: aSystemAnnouncement
]

{ #category : #converting }
ClyVariableQuery >> withMetaLevelScope: aScopeClass [ 
	^self withScope: (scope withMetaLevel: aScopeClass)
]
