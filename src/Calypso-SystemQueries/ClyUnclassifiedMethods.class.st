"
I return all unclassified methods.
"
Class {
	#name : #ClyUnclassifiedMethods,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #printing }
ClyUnclassifiedMethods >> description [

	^'unclassified methods'
]

{ #category : #testing }
ClyUnclassifiedMethods >> isResult: aQueryResult affectedBy: aSystemAnnouncement [
	
	^aSystemAnnouncement affectsMethods
		and: [ scope includesMethodsAffectedBy: aSystemAnnouncement]
]

{ #category : #testing }
ClyUnclassifiedMethods >> selectsMethod: aMethod [
	^aMethod tags isEmpty
]
