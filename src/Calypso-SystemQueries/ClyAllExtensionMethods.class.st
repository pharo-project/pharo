"
I return only extension methods.
"
Class {
	#name : #ClyAllExtensionMethods,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #printing }
ClyAllExtensionMethods >> description [

	^'extension methods'
]

{ #category : #'system changes' }
ClyAllExtensionMethods >> isResult: aQueryResult affectedBy: aSystemAnnouncement [
	
	^aSystemAnnouncement affectsClassExtension
		and: [ scope includesMethodsAffectedBy: aSystemAnnouncement]
]

{ #category : #testing }
ClyAllExtensionMethods >> selectsMethod: aMethod [
	^aMethod isExtension
]
