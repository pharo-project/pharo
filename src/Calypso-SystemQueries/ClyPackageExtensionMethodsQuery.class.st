"
I return all methods from the scope which are extended by particular package.

To instantiate my instances use following expressions: 

	ClyPackageExtensionMethods of: aPackage.
	ClyPackageExtensionMethods of: aPackage from: aScope
 
Internal Representation and Key Implementation Points.

    Instance Variables
	package:		<RPackage>

"
Class {
	#name : #ClyPackageExtensionMethodsQuery,
	#superclass : #ClyMethodQuery,
	#instVars : [
		'package'
	],
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #'instance creation' }
ClyPackageExtensionMethodsQuery class >> of: aPackage [

	^self new 
		package: aPackage
]

{ #category : #'instance creation' }
ClyPackageExtensionMethodsQuery class >> of: aPackage from: aScope [

	^(self from: aScope)
		package: aPackage
]

{ #category : #comparing }
ClyPackageExtensionMethodsQuery >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
	^package = anObject package
]

{ #category : #printing }
ClyPackageExtensionMethodsQuery >> description [

	^'extension methods of ', package name
]

{ #category : #comparing }
ClyPackageExtensionMethodsQuery >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ super hash bitXor: package hash
]

{ #category : #'system changes' }
ClyPackageExtensionMethodsQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [
	
	^(aSystemAnnouncement affectsMethodsDefinedInPackage: package)
		and: [ scope includesMethodsAffectedBy: aSystemAnnouncement]
]

{ #category : #accessing }
ClyPackageExtensionMethodsQuery >> package [
	^ package
]

{ #category : #accessing }
ClyPackageExtensionMethodsQuery >> package: anObject [
	package := anObject
]

{ #category : #printing }
ClyPackageExtensionMethodsQuery >> printExtraInfoOn: aStream [
	super printExtraInfoOn: aStream.

	^aStream nextPutAll: package name
]

{ #category : #testing }
ClyPackageExtensionMethodsQuery >> selectsMethod: aMethod [
	^aMethod isExtension and: [ aMethod package = package ]
]
