"
I return only extension methods.
"
Class {
	#name : 'ClyAllExtensionMethodsQuery',
	#superclass : 'ClyMethodQuery',
	#category : 'Calypso-SystemQueries-Queries',
	#package : 'Calypso-SystemQueries',
	#tag : 'Queries'
}

{ #category : 'printing' }
ClyAllExtensionMethodsQuery >> description [

	^'extension methods'
]

{ #category : 'system changes' }
ClyAllExtensionMethodsQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [

	^aSystemAnnouncement affectsClassExtension
		and: [ scope includesMethodsAffectedBy: aSystemAnnouncement]
]

{ #category : 'testing' }
ClyAllExtensionMethodsQuery >> selectsMethod: aMethod [
	^aMethod isExtension
]
