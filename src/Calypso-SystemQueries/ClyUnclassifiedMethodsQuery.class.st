"
I return all unclassified methods.
"
Class {
	#name : #ClyUnclassifiedMethodsQuery,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #printing }
ClyUnclassifiedMethodsQuery >> description [

	^'unclassified methods'
]

{ #category : #testing }
ClyUnclassifiedMethodsQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [
	
	^aSystemAnnouncement affectsMethods
		and: [ scope includesMethodsAffectedBy: aSystemAnnouncement]
]

{ #category : #testing }
ClyUnclassifiedMethodsQuery >> selectsMethod: aMethod [
	^aMethod tags isEmpty
]
