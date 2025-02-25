"
I return all unclassified methods.
"
Class {
	#name : 'ClyUnclassifiedMethodsQuery',
	#superclass : 'ClyMethodQuery',
	#category : 'Calypso-SystemQueries-Queries',
	#package : 'Calypso-SystemQueries',
	#tag : 'Queries'
}

{ #category : 'printing' }
ClyUnclassifiedMethodsQuery >> description [

	^ 'unclassified methods'
]

{ #category : 'testing' }
ClyUnclassifiedMethodsQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [

	^ aSystemAnnouncement affectsMethods and: [ scope includesMethodsAffectedBy: aSystemAnnouncement ]
]

{ #category : 'testing' }
ClyUnclassifiedMethodsQuery >> selectsMethod: aMethod [
	"The protocol can be nil if the method was removed from the system"

	^ aMethod protocol isNotNil and: [ aMethod isClassified not ]
]
