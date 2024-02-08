Extension { #name : 'PackageRenamed' }

{ #category : '*Calypso-SystemQueries' }
PackageRenamed >> affectsClass: aClass [
	^package == aClass package
]

{ #category : '*Calypso-SystemQueries' }
PackageRenamed >> affectsMethod: aMethod [
	^package == aMethod package
]

{ #category : '*Calypso-SystemQueries' }
PackageRenamed >> affectsMethods [
	"methods belong to package which could be represented by tools as part of method"
	^true
]

{ #category : '*Calypso-SystemQueries' }
PackageRenamed >> affectsMethodsDefinedInClass: aClass [

	^ (package includesClass: aClass) or: [ package extendsClass: aClass ]
]

{ #category : '*Calypso-SystemQueries' }
PackageRenamed >> affectsMethodsDefinedInPackage: aPackage [

	^package == aPackage
]

{ #category : '*Calypso-SystemQueries' }
PackageRenamed >> canAffectResultOfMethodQuery: aMethodQuery [
	^true
]
